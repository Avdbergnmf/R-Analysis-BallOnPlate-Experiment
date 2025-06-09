# ─────────────────────────────────────────────────────────────────────────────
# Dependencies
# ─────────────────────────────────────────────────────────────────────────────
library(dplyr)
library(tibble)

# ─────────────────────────────────────────────────────────────────────────────
# 1) Build a plate trace: gives t, p (pos), pd (vel), pdd (acc)
# ─────────────────────────────────────────────────────────────────────────────
build_plate_trace <- function(p_fun, t_end, dt) {
  stopifnot(is.function(p_fun), dt > 0, t_end > 0)
  t <- seq(0, t_end, by = dt)
  p <- p_fun(t)

  pd <- c(diff(p) / dt, 0)
  pdd <- c(diff(pd) / dt, 0)

  tibble(t = t, p = p, pd = pd, pdd = pdd)
}

# ─────────────────────────────────────────────────────────────────────────────
# 2) Post‐process: compute world‐space velocities & accelerations + energy related metrics
# ─────────────────────────────────────────────────────────────────────────────
post_process_df <- function(df) {
  # forward-difference, pad with 0 for the first element
  dt <- c(diff(df$t), NA) # NA for the padding row
  dx <- c(diff(df$x), NA)
  dy <- c(diff(df$y), NA)

  vx <- dx / dt # same length as df
  vy <- dy / dt

  ax <- c(diff(vx), NA) / dt # dt is one element shorter inside diff()
  ay <- c(diff(vy), NA) / dt

  # replace the NA’s in the first row with zeros (or just keep NA)
  vx[1] <- vy[1] <- ax[1] <- ay[1] <- 0

  df %>%
    mutate(
      vx = vx, vy = vy,
      ax = ax, ay = ay
    )
}

#  Specific mechanical energy  (per unit mass) - assumes postprocess already ran
add_energy_cols <- function(df, g = 9.81) {
  df %>% mutate(
    ke = 0.5 * (vx^2 + vy^2), # kinetic
    pe = g * y, # potential (y already world-frame)
    e  = ke + pe # total
  )
}

#  Specific mechanical power and cumulative work  - assumes postprocess already ran
add_power_cols <- function(df) {
  df %>% mutate(
    p_instant = ax * vx + ay * vy, # W kg⁻¹
    work      = cumsum(p_instant * c(0, diff(t))) # J kg⁻¹
  )
}

#  Safety margin: energy distance to escape  - assumes postprocess already ran
add_safety_cols <- function(df, shape) {
  # potential energy the ball would have on the rim
  xy_max <- shape$getXY(shape$getMaxQ())
  e_escape <- shape$g * xy_max[2] # per kg
  df %>% mutate(
    margin_E = e_escape - e, # J kg⁻¹ still “in the bank”
    danger   = -margin_E / e_escape # pmax(0, -margin_E) / e_escape  # 0 (safe) … 1 (escaped)
  )
}

kalman_cv_filter <- function(pos,
                             dt = 0.005, # scalar or vector Δt (s)
                             sigma_a = 1.0, # process noise  (m/s²)
                             sigma_m = 0.00001) { # meas. noise     (m)
  n <- length(pos)
  if (n == 0L) {
    return(list(p = numeric(0), v = numeric(0)))
  }

  ## handle dt vector vs scalar -------------------------------------------
  if (length(dt) == 1L) {
    dt <- rep(dt, n)
  } else if (length(dt) != n) stop("dt must be length 1 or length(pos)")

  ## result vectors --------------------------------------------------------
  p_hat <- numeric(n) # filtered position
  v_hat <- numeric(n) # filtered velocity

  ## initial state ---------------------------------------------------------
  p_hat[1] <- ifelse(is.na(pos[1]), 0, pos[1])
  v_hat[1] <- 0

  ## initial covariance (large = uninformative) ---------------------------
  P11 <- 1 # var(p)
  P12 <- 0 # cov(p,v)
  P22 <- 1 # var(v)

  for (k in 2:n) {
    dtk <- dt[k]

    ## ---------- PREDICT --------------------------------------------------
    p_pred <- p_hat[k - 1] + v_hat[k - 1] * dtk
    v_pred <- v_hat[k - 1]

    Q11 <- sigma_a^2 * dtk^3 / 3
    Q12 <- sigma_a^2 * dtk^2 / 2
    Q22 <- sigma_a^2 * dtk

    P11p <- P11 + (P12 + P12 + P22) * dtk + Q11 # F P Fᵀ + Q
    P12p <- P12 + P22 * dtk + Q12
    P22p <- P22 + Q22

    ## ---------- UPDATE (only if sample is present) -----------------------
    if (!is.na(pos[k])) {
      y <- pos[k] - p_pred # innovation
      S <- P11p + sigma_m^2 # innovation covariance
      K1 <- P11p / S # Kalman gains
      K2 <- P12p / S

      p_hat[k] <- p_pred + K1 * y
      v_hat[k] <- v_pred + K2 * y

      P11 <- P11p - K1 * P11p
      P12 <- P12p - K1 * P12p
      P22 <- P22p - K2 * P12p
    } else { # no measurement → prediction only
      p_hat[k] <- p_pred
      v_hat[k] <- v_pred
      P11 <- P11p
      P12 <- P12p
      P22 <- P22p
    }
  }

  list(p = p_hat, v = v_hat)
}


patch_gaps <- function(time, pos, tol = 1.5) {
  dt_nom <- median(diff(time))
  gap <- which(diff(time) > tol * dt_nom)
  if (!length(gap)) {
    return(list(time = time, pos = pos))
  }

  for (j in rev(gap)) {
    n <- round(diff(time)[j] / dt_nom) - 1
    t_fill <- seq(time[j] + dt_nom, by = dt_nom, length.out = n)
    p_fill <- seq(pos[j], pos[j + 1], length.out = n + 2)[-c(1, n + 2)]
    time <- append(time, t_fill, after = j)
    pos <- append(pos, p_fill, after = j)
  }
  list(time = time, pos = pos)
}

# ─────────────────────────────────────────────────────────────────────────────
# 3) Profile‐driven simulator
#     - shape: an R6 ProfileShape (implements getXY, getSlope, getMaxQ, mu_d, mu_s, g)
#     - q0, qd0: initial arc‐length and speed
#     - plate_acc(t) OR plate_trace (t,p,pd,pdd)
#     - returns tibble with t, q, qd, qdd, p, pd, pdd, esc, x, y, vx, vy, ax, ay
# ─────────────────────────────────────────────────────────────────────────────
simulate_profile_core <- function(shape,
                                  q0 = 0, qd0 = 0,
                                  plate_acc = NULL,
                                  plate_trace = NULL,
                                  t_end = 10, dt = 0.01,
                                  breakOnEscaping = FALSE) {
  # sanity
  if (is.null(plate_acc) && is.null(plate_trace)) {
    stop("Must supply plate_acc(t) or plate_trace.")
  }

  # iteration count
  if (!is.null(plate_trace)) {
    plate_trace <- arrange(plate_trace, t)
    n <- nrow(plate_trace)
  } else {
    n <- ceiling(t_end / dt) + 1
  }

  # storage
  out <- vector("list", n)

  # initial state
  t <- 0
  q <- q0
  qd <- qd0
  p <- 0
  pd <- 0
  pdd <- 0
  esc <- FALSE
  qdd <- 0

  # helper to save each row
  save_row <- function(i) {
    # world‐space position
    xy <- shape$getXY(q)
    x <- p + xy[1]
    y <- xy[2]

    row <- tibble(
      t   = t,
      q   = q,
      qd  = qd,
      qdd = qdd,
      p   = p,
      pd  = pd,
      pdd = pdd,
      esc = esc,
      x   = x,
      y   = y
    )
    out[[i]] <<- row
  }

  # main loop
  for (i in seq_len(n)) {
    # 1) plate kinematics
    if (!is.null(plate_trace)) {
      p <- plate_trace$p[i]
      pd <- plate_trace$pd[i]
      pdd <- plate_trace$pdd[i]
      t <- plate_trace$t[i]
      dt_i <- if (i == 1) dt else plate_trace$t[i] - plate_trace$t[i - 1]
    } else {
      pdd <- plate_acc(t)
      pd <- pd + pdd * dt
      p <- p + pd * dt
      dt_i <- dt
    }

    # 2) shape geometry at q
    slope <- shape$getSlope(q)
    theta <- atan(slope) # tangent angle
    # tangential acc: gravity + plate drive
    a_t <- -shape$g * sin(theta) - pdd * cos(theta)

    # 3) friction
    N <- shape$g * cos(theta)
    moving <- abs(qd) > 1e-4
    maxStatic <- shape$mu_s * N
    if (moving) {
      a_t <- a_t - shape$mu_d * N * sign(qd)
      # 3.5) damping
      a_t <- a_t - shape$damping * qd
    } else if (abs(a_t) < maxStatic) {
      a_t <- 0
      qd <- 0
    }

    # 4) integrate
    qdd <- a_t
    qd <- qd + qdd * dt_i
    q <- q + qd * dt_i

    # 5) escape?
    esc <- abs(q) > shape$getMaxQ()

    # 6) save
    save_row(i)

    if (esc && breakOnEscaping) break
    if (is.null(plate_trace)) t <- t + dt
  }

  # compile & post‐process
  df <- bind_rows(out)
  df <- post_process_df(df)

  df <- post_process_df(df)
  df <- add_energy_cols(df, g = shape$g)
  df <- add_power_cols(df)
  df <- add_safety_cols(df, shape)

  df
}
