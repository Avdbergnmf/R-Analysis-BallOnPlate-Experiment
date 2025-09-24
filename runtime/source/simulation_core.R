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

#' Compute velocity components from arc-length velocity using shape slope
#' @param df Dataframe with q, qd, and arcDeg columns
#' @return df with vx_from_qd and vy_from_qd columns added
compute_velocities_from_qd <- function(df) {
  # Initialize columns
  df$vx_from_qd <- 0
  df$vy_from_qd <- 0

  # Only process real simulation data (not artificial gap points)
  if ("simulating" %in% colnames(df)) {
    real_data_indices <- which(df$simulating)
  } else {
    real_data_indices <- 1:nrow(df)
  }

  if (length(real_data_indices) == 0) {
    return(df)
  }

  # Process each unique arcDeg separately
  unique_arcDegs <- unique(df$arcDeg[real_data_indices])

  for (arcDeg in unique_arcDegs) {
    # Create shape for this arcDeg
    shape <- CircularProfileShape$new(
      R = 5.0, arcDeg = arcDeg, L = 0,
      mu_d = 0.03, mu_s = 0.05, damping = 0, g = 9.81
    )

    # Get rows with this arcDeg and real data
    rows_with_arcDeg <- intersect(which(df$arcDeg == arcDeg), real_data_indices)

    if (length(rows_with_arcDeg) > 0) {
      q_values <- df$q[rows_with_arcDeg]
      qd_values <- df$qd[rows_with_arcDeg]

      # Compute velocities using shape slope
      velocities <- t(sapply(seq_along(q_values), function(i) {
        q_val <- q_values[i]
        qd_val <- qd_values[i]

        # Get slope at this q position
        slope <- shape$getSlope(q_val)

        # Convert slope to angle (atan2 gives angle from x-axis)
        angle <- atan2(slope, 1) # slope = dy/dx, so angle = atan2(dy, dx)

        # Decompose qd into x and y components
        # qd is the magnitude of velocity along the arc
        # vx = qd * cos(angle), vy = qd * sin(angle)
        vx <- qd_val * cos(angle)
        vy <- qd_val * sin(angle)

        c(vx, vy)
      }))

      df$vx_from_qd[rows_with_arcDeg] <- velocities[, 1]
      df$vy_from_qd[rows_with_arcDeg] <- velocities[, 2]
    }
  }

  return(df)
}

post_process_df <- function(df) {
  # Drop invalid samples with non-positive or missing dt to avoid divide-by-zero
  remove_mask <- is.na(df$dt) | df$dt < 0.001
  removed_n <- sum(remove_mask, na.rm = TRUE)
  if (removed_n > 0) {
    cat(sprintf("[INFO] post_process_df: removed %d samples with non-positive/NA dt.\n", removed_n))
    df <- df[!remove_mask, , drop = FALSE]
  }

  # First compute velocities from qd (cleaner signal)
  df <- compute_velocities_from_qd(df)

  # forward-difference, pad with 0 for the first element
  dt <- df$dt

  # COMPARISON: Also compute old method for debugging
  dx_old <- c(diff(df$x), NA)
  dy_old <- c(diff(df$y), NA)
  vx_old <- dx_old / dt
  vy_old <- dy_old / dt
  vx_old[length(vx_old)] <- 0 # Set last element to 0
  vy_old[length(vy_old)] <- 0

  # Use the cleaner qd-based velocities as primary
  vx <- df$vx_from_qd
  vy <- df$vy_from_qd

  # For world velocities, we still need to add plate motion
  vx_world <- vx + df$pVel

  # Accelerations from the cleaner velocities
  ax <- c(diff(vx), NA) / dt
  ay <- c(diff(vy), NA) / dt

  plate_acc <- df$pAcc
  ax_world <- ax + plate_acc

  # replace the NA's in the last row with zeros
  ax[length(ax)] <- ax_world[length(ax_world)] <- ay[length(ay)] <- 0

  df %>%
    mutate(
      # Local coordinates (plate-relative, computed from clean qd)
      vx = vx, vy = vy,
      ax = ax, ay = ay,
      # World coordinates (include plate motion)
      vx_world = vx_world,
      ax_world = ax_world
    ) %>%
    select(-vx_from_qd, -vy_from_qd) # Remove intermediate columns
}

#  Specific mechanical energy  (per unit mass) - assumes postprocess already ran
add_energy_cols <- function(df, g = 9.81) {
  df %>% mutate(
    # World energy (includes plate motion)
    ke_world = 0.5 * (vx_world^2 + vy^2), # kinetic in world frame
    pe = g * y, # potential (y already world-frame)
    e_world = ke_world + pe, # total world energy

    # Local energy (plate-relative, more relevant for escape)
    ke = 0.5 * (vx^2 + vy^2), # kinetic relative to plate
    e = ke + pe # total plate-relative energy (this is what matters for escape)
  )
}

#  Specific mechanical power and cumulative work  - assumes postprocess already ran
add_power_cols <- function(df) {
  df %>% mutate(
    # Plate power and work
    power_plate = pAcc * pVel, # W kg⁻¹ plate power
    work_plate = cumsum(abs(power_plate) * dt), # J kg⁻¹ cumulative work in world frame

    # World power (includes plate motion effects)
    power_world = ax_world * vx_world + ay * vy, # W kg⁻¹ in world frame
    work_world = cumsum(abs(power_world) * dt), # J kg⁻¹ cumulative work in world frame

    # Local power (plate-relative, more physically meaningful)
    power = ax * vx + ay * vy, # W kg⁻¹ relative to plate
    work = cumsum(abs(power) * dt) # J kg⁻¹ cumulative work relative to plate
  )
}

#  Safety margin: energy distance to escape  - assumes postprocess already ran
add_safety_cols <- function(df, shape) {
  # Check if q_max column exists (should be pre-computed)
  if (!"q_max" %in% colnames(df)) {
    stop("q_max column not found. It should be computed in calculate_coordinates().")
  }

  # Use physical constants from the shape object
  mu_d <- shape$mu_d # Coulomb friction coefficient
  damping <- shape$damping # Viscous damping coefficient
  g <- shape$g # Gravity
  R <- shape$R # Radius (hardcoded since it's consistent)

  df %>% mutate(
    # Calculate escape height for each row based on its q_max
    # For CircularProfileShape: y_escape = R * (1 - cos(q_max/R))
    # Since q_max = R * ThetaMax, we have q_max/R = ThetaMax
    y_escape = R * (1 - cos(q_max / R)),

    # Distance to nearest escape point (shortest path)
    dist_to_escape = pmax(0, q_max - abs(q)),

    # Energy required to reach escape point (potential energy difference)
    e_potential_needed = g * (y_escape - y),

    # Energy losses during travel to escape:

    # 1. Coulomb friction loss: W = μ_d * g * distance
    e_loss_friction = mu_d * g * dist_to_escape,

    # 2. Viscous damping loss: approximate using current velocity
    # Assume average velocity ≈ current |qd|, time = distance/velocity
    # Energy lost ≈ damping * qd^2 * time = damping * |qd| * distance
    qd_abs = abs(qd),
    e_loss_damping = ifelse(qd_abs > 1e-6, # Avoid division by zero
      damping * qd_abs * dist_to_escape,
      0
    ),

    # Total energy needed to escape (potential + losses)
    e_total_needed = e_potential_needed + e_loss_friction + e_loss_damping,

    # Energy margin: how much energy we have vs what's needed
    # Positive = safe (current energy < needed)
    # Negative = can escape (current energy > needed)
    margin_E = e_total_needed - e,

    # Danger level: how much excess energy we have relative to what's needed
    # 0 = safe, 1 = just enough to escape, >1 = excess energy beyond escape
    danger = pmax(0, ifelse(e_total_needed > 1e-6, # Avoid division by zero
      (e - e_total_needed) / e_total_needed,
      0
    )),

    # Time to escape: given current velocity, how long until ball falls off
    # Calculate distance to the edge the ball is moving toward
    # qd > 0: moving right, distance = q_max - q
    # qd < 0: moving left, distance = q_max + q  
    # qd ≈ 0: max 10s
    time_to_escape = ifelse(
      abs(qd) < 1e-6, # No velocity
      10,# max 10s
      ifelse(
        qd > 0, # Moving right toward positive edge
        pmin(10, (q_max - q) / abs(qd)),
        # Moving left toward negative edge  
        pmin(10, (q_max + q) / abs(qd))
      )
    )
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
    x <- xy[1] # position relative to plate center
    x_world <- p + x # position in world coordinates
    y <- xy[2] # height (same in both frames)

    row <- tibble(
      t = t,
      q = q,
      qd = qd,
      qdd = qdd,
      p = p,
      pd = pd,
      pdd = pdd,
      esc = esc,
      x = x,
      x_world = x_world,
      y = y
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
