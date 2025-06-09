# ─────────────────────────────────────────────────────────────────────────────
# ProfileShapes for Marble-on-Track Simulator
# Requires simulation_core.R for simulate_profile_core()
# ─────────────────────────────────────────────────────────────────────────────

library(R6)
library(dplyr)
library(tibble)

#' Plot an R6 ProfileShape
#'
#' @param shape   An R6 object inheriting ProfileShape (e.g. CircularProfileShape$new(...))
#' @param res     Number of sample points along arc‐length (default 201)
#' @export
plot_profile_shape <- function(shape, res = 201) {
  q_max <- shape$getMaxQ()
  qs    <- seq(-q_max, q_max, length.out = res)
  xy_mat <- t(vapply(qs, shape$getXY, numeric(2)))
  df <- data.frame(q = qs, x = xy_mat[,1], y = xy_mat[,2])
  plot(df$x, df$y, type = "l", lwd = 2,
       xlab = "x (track frame)", ylab = "y (m)",
       main = paste0("Profile: ", class(shape)[1]),
       asp = 1)
  abline(h = 0, col = "gray")
  invisible(df)
}

# ─────────────────────────────────────────────────────────────────────────────
# Abstract base class
# ─────────────────────────────────────────────────────────────────────────────
ProfileShape <- R6Class("ProfileShape",
                        public = list(
                          mu_d = NULL,
                          mu_s = NULL,
                          damping = NULL,
                          g    = NULL,
                          
                          initialize = function(mu_d = 0.05, mu_s = 0.07, damping = 0, g = 9.81) {
                            self$mu_d <- mu_d
                            self$mu_s <- mu_s
                            self$damping <- damping
                            self$g    <- g
                          },
                          
                          # half-span in arc-length
                          getMaxQ = function() stop("getMaxQ() not implemented"),
                          # (x,y) at signed arc-length q
                          getXY   = function(q) stop("getXY() not implemented"),
                          # slope dy/dx at q
                          getSlope = function(q) stop("getSlope() not implemented")
                        )
)

# ─────────────────────────────────────────────────────────────────────────────
# Circular (cup) profile
# ─────────────────────────────────────────────────────────────────────────────
CircularProfileShape <- R6Class(
  "CircularProfileShape",
  inherit = ProfileShape,
  public = list(
    R      = NULL,
    arcDeg = NULL,
    L      = NULL,          # ramp length (m)
    
    initialize = function(R = 1, arcDeg = 180, L = 0,
                          mu_d = .03, mu_s = .05,
                          damping = 0, g = 9.81) {
      super$initialize(mu_d, mu_s, damping, g)
      self$R      <- R
      self$arcDeg <- arcDeg
      self$L      <- L
    },
    
    ThetaMax = function() (self$arcDeg * pi / 180) / 2,
    
    # total usable arc-length (arc + ramp)
    getMaxQ = function() self$R * self$ThetaMax() + self$L,
    
    # (x,y) for any q  ∈ [-qMax, qMax]
    getXY = function(q) {
      thetaMax <- self$ThetaMax()
      qArc     <- self$R * thetaMax
      signQ    <- sign(q)
      
      if (abs(q) <= qArc) {                         # inside circular part
        phi <- q / self$R
        x   <- self$R * sin(phi)
        y   <- self$R * (1 - cos(phi))
      } else {                                      # on the straight ramp
        s   <- abs(q) - qArc                        # distance along ramp
        phi <- thetaMax * signQ                     # rim tangent angle
        x0  <- self$R * sin(phi)                    # rim XY
        y0  <- self$R * (1 - cos(phi))
        x   <- x0 + signQ * s * cos(phi)
        y   <- y0 + signQ * s * sin(phi)
      }
      c(x, y)
    },
    
    # dy/dx  (slope) for any q
    getSlope = function(q) {
      thetaMax <- self$ThetaMax()
      qArc     <- self$R * thetaMax
      
      if (abs(q) <= qArc) {
        phi <- q / self$R
        tan(phi)
      } else {
        tan(thetaMax) * sign(q)   # constant on ramp
      }
    }
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# Flat→Transition→Ramp profile
# ─────────────────────────────────────────────────────────────────────────────
FlatTransitionRampProfileShape <- R6Class("FlatTransitionRampProfileShape",
                                          inherit = ProfileShape,
                                          public = list(
                                            W      = NULL,
                                            L      = NULL,
                                            T      = NULL,
                                            phiDeg = NULL,
                                            
                                            initialize = function(W = 0.15, L = 0.1, T = 0.05, phiDeg = 15,
                                                                  mu_d = 0.04, mu_s = 0.06, damping = 0, g = 9.81) {
                                              super$initialize(mu_d, mu_s, damping, g)
                                              self$W      <- W
                                              self$L      <- L
                                              self$T      <- min(max(0, T), L)
                                              self$phiDeg <- phiDeg
                                            },
                                            
                                            phiRad = function() self$phiDeg * pi / 180,
                                            R_t    = function() if (self$T > 0) self$T / self$phiRad() else 0,
                                            
                                            getMaxQ = function() self$W + self$L,
                                            
                                            getXY = function(q) {
                                              qAbs <- abs(q); sgn <- sign(q)
                                              if (qAbs <= self$W) return(c(q, 0))
                                              if (qAbs <= self$W + self$T) {
                                                s    <- qAbs - self$W
                                                phi  <- s / self$R_t()
                                                xoff <- self$R_t() * sin(phi)
                                                y    <- self$R_t() * (1 - cos(phi))
                                                return(c(sgn * (self$W + xoff), y))
                                              }
                                              s2 <- qAbs - (self$W + self$T)
                                              x_t <- self$R_t() * sin(self$phiRad())
                                              y_t <- self$R_t() * (1 - cos(self$phiRad()))
                                              x <- self$W + x_t + s2 * cos(self$phiRad())
                                              y <- y_t   + s2 * sin(self$phiRad())
                                              c(sgn * x, y)
                                            },
                                            
                                            getSlope = function(q) {
                                              qAbs <- abs(q); sgn <- sign(q)
                                              if (qAbs <= self$W) return(0)
                                              if (qAbs <= self$W + self$T) {
                                                phi <- (qAbs - self$W) / self$R_t()
                                                return(sgn * tan(phi))
                                              }
                                              return(sgn * tan(self$phiRad()))
                                            }
                                          )
)

# ─────────────────────────────────────────────────────────────────────────────
# Cosine Profile Shape with Linear Extensions
# ─────────────────────────────────────────────────────────────────────────────
CosineProfileShape <- R6Class("CosineProfileShape",
                              inherit = ProfileShape,
                              public = list(
                                W = NULL,      # Width scaling factor
                                H = NULL,      # Height scaling factor
                                n = NULL,      # Number of pi radians to extend in each direction
                                L = NULL,      # Length of linear extension after cosine
                                
                                initialize = function(W = 1, H = 0.5, n = 1, L = 0.5,
                                                      mu_d = 0.03, mu_s = 0.05, damping = 0, g = 9.81) {
                                  super$initialize(mu_d, mu_s, damping, g)
                                  self$W <- W
                                  self$H <- H
                                  self$n <- n
                                  self$L <- L
                                },
                                
                                # Get the scaled x-coordinate from the parameter t
                                # t ranges from -n*pi to n*pi
                                getScaledX = function(t) {
                                  self$W * t / (self$n * pi)
                                },
                                
                                # Get the cosine y-coordinate from parameter t
                                getScaledY = function(t) {
                                  self$H * (1 - cos(t)) / 2
                                },
                                
                                # Calculate the arc length from 0 to t for the cosine curve
                                # This uses the analytical formula for arclength of cosine
                                getCosineArcLength = function(t) {
                                  # Scale factor is sqrt(W^2/(n*pi)^2 + (H/2)^2)
                                  a <- self$W / (self$n * pi)
                                  b <- self$H / 2
                                  
                                  # For a curve x=a*t, y=b*sin(t), the arc length is:
                                  # integrate(sqrt(a^2 + b^2*cos^2(t)), t)
                                  # This evaluates to:
                                  # sqrt(a^2 + b^2) * E(t, b^2/(a^2 + b^2))
                                  # where E is the elliptic integral of the second kind
                                  
                                  # We use the approximation based on the binomial series:
                                  # sqrt(a^2 + b^2*cos^2(t)) ≈ sqrt(a^2 + b^2/2) + (b^2/2)*cos(2*t)/(2*sqrt(a^2 + b^2/2))
                                  
                                  m <- b^2 / (2 * (a^2 + b^2/2))
                                  c <- sqrt(a^2 + b^2/2)
                                  
                                  # The integral of this approximation gives:
                                  arclength <- c * t + (b^2 / (4 * c)) * sin(2 * t)
                                  return(arclength)
                                },
                                
                                # Get the total arc length of the cosine portion
                                getCosinePartMaxQ = function() {
                                  # Calculate the arc length from 0 to n*pi (one side)
                                  self$getCosineArcLength(self$n * pi) * 2
                                },
                                
                                # Calculate the slope at the end of the cosine (t = ±n*pi)
                                getEndSlope = function() {
                                  # Derivative of cosine at t = n*pi
                                  dx_dt <- self$W / (self$n * pi)  # dx/dt = W/(n*pi)
                                  dy_dt <- self$H * sin(self$n * pi) / 2  # dy/dt = (H/2) * sin(t)
                                  
                                  # At t = n*pi, sin(n*pi) = 0 for integer n
                                  # For non-integer n, we calculate the actual slope
                                  if (abs(sin(self$n * pi)) < 1e-10) {
                                    # If n is integer, the slope is approximately 0
                                    return(0)
                                  } else {
                                    return(dy_dt / dx_dt)
                                  }
                                },
                                
                                getMaxQ = function() {
                                  # Total arc length: cosine part + linear extensions
                                  cosine_part <- self$getCosinePartMaxQ()
                                  return(cosine_part/2 + self$L)
                                },
                                
                                # Map from arc length q to parameter t and position
                                getXY = function(q) {
                                  qAbs <- abs(q)
                                  sgn <- sign(q)
                                  
                                  # Maximum arc length for cosine part (one side)
                                  cosine_max_q <- self$getCosinePartMaxQ() / 2
                                  
                                  if (qAbs <= cosine_max_q) {
                                    # We're in the cosine part
                                    # Need to find t from q by inverting the arc length function
                                    # This requires numerical method, but we can approximate
                                    
                                    # Use a simple numerical approach to find t for a given q
                                    # Start with a linear approximation
                                    a <- self$W / (self$n * pi)
                                    b <- self$H / 2
                                    c <- sqrt(a^2 + b^2/2)
                                    
                                    # Initial guess for t based on linear approximation
                                    t_approx <- qAbs / c
                                    if (sgn < 0) t_approx <- -t_approx
                                    
                                    # Refine t_approx using Newton's method (a few iterations)
                                    for (i in 1:3) {
                                      arc_length <- self$getCosineArcLength(abs(t_approx)) * sign(t_approx)
                                      arc_length_prime <- sqrt((a^2 + b^2/2) + (b^2/2) * cos(2*t_approx))
                                      t_approx <- t_approx - (arc_length - q) / arc_length_prime
                                    }
                                    
                                    # Now we have t, compute x and y
                                    x <- self$getScaledX(t_approx)
                                    y <- self$getScaledY(t_approx)
                                    return(c(x, y))
                                  } else {
                                    # We're in the linear extension part
                                    # Get the position at the end of cosine
                                    t_end <- sgn * self$n * pi
                                    x_end <- self$getScaledX(t_end)
                                    y_end <- self$getScaledY(t_end)
                                    
                                    # Get the slope at the end
                                    slope <- self$getEndSlope()
                                    
                                    # Calculate distance along the linear extension
                                    extra_q <- qAbs - cosine_max_q
                                    
                                    # Calculate extension using parametric form with unit length direction
                                    # For a line with slope m, the unit direction vector is (1, m)/sqrt(1 + m^2)
                                    dir_norm <- sqrt(1 + slope^2)
                                    dx <- extra_q / dir_norm
                                    dy <- slope * dx
                                    
                                    return(c(x_end + sgn * dx, y_end + dy))
                                  }
                                },
                                
                                getSlope = function(q) {
                                  qAbs <- abs(q)
                                  sgn <- sign(q)
                                  
                                  # Maximum arc length for cosine part (one side)
                                  cosine_max_q <- self$getCosinePartMaxQ() / 2
                                  
                                  if (qAbs <= cosine_max_q) {
                                    # We're in the cosine part
                                    # Similar to getXY, first find t from q
                                    a <- self$W / (self$n * pi)
                                    b <- self$H / 2
                                    c <- sqrt(a^2 + b^2/2)
                                    
                                    # Initial guess for t based on linear approximation
                                    t_approx <- qAbs / c
                                    if (sgn < 0) t_approx <- -t_approx
                                    
                                    # Refine t_approx using Newton's method
                                    for (i in 1:3) {
                                      arc_length <- self$getCosineArcLength(abs(t_approx)) * sign(t_approx)
                                      arc_length_prime <- sqrt((a^2 + b^2/2) + (b^2/2) * cos(2*t_approx))
                                      t_approx <- t_approx - (arc_length - q) / arc_length_prime
                                    }
                                    
                                    # Compute slope at t
                                    # dx/dt = W/(n*pi), dy/dt = (H/2)*sin(t)
                                    # slope = (dy/dt)/(dx/dt) = (H*n*pi)/(2*W) * sin(t)
                                    return((self$H * self$n * pi)/(2 * self$W) * sin(t_approx))
                                  } else {
                                    # In the linear extension part, slope is constant
                                    return(sgn * self$getEndSlope())
                                  }
                                }
                              )
)

#cosine_shape <- CosineProfileShape$new(W = 1, H = 1, n = 0.5, L = 0.5)
#plot_profile_shape(cosine_shape)