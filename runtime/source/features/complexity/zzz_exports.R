#' Complexity Module Export Management
#'
#' This file finalizes the complexity feature by collecting all internal
#' implementation details into a private environment and exposing only the
#' supported public API surface. This keeps the module self-contained and
#' prevents other feature modules from depending on internal helpers.

complexity <- local({
  feature_env <- parent.env(environment())

  # Logging helpers are injected by the loader; keep track so we can remove them
  reserved_names <- c("create_logger", "create_module_logger")

  # Capture everything else in a private environment so consumers cannot reach it
  private_env <- base::new.env(parent = .GlobalEnv)
  internal_names <- setdiff(base::ls(envir = feature_env, all.names = TRUE), c(reserved_names, "complexity"))

  for (name in internal_names) {
    obj <- base::get(name, envir = feature_env, inherits = FALSE)
    base::assign(name, obj, envir = private_env)
  }

  # Ensure all functions look up dependencies inside the private environment
  for (name in internal_names) {
    obj <- base::get(name, envir = private_env, inherits = FALSE)
    if (is.function(obj)) {
      environment(obj) <- private_env
      base::assign(name, obj, envir = private_env)
    }
  }

  # Remove internal symbols from the feature environment before export
  if (length(internal_names) > 0) {
    base::rm(list = internal_names, envir = feature_env)
  }

  # Remove injected helpers so they are not re-exported (modules can still access the
  # global versions provided by logging.R through the parent environment).
  for (name in reserved_names) {
    if (base::exists(name, envir = feature_env, inherits = FALSE)) {
      base::rm(list = name, envir = feature_env)
    }
  }

  # Helper to safely invoke private functions (guards against missing entries)
  call_private <- function(fn_name, ...) {
    if (!base::exists(fn_name, envir = private_env, inherits = FALSE)) {
      stop(sprintf("Complexity API function '%s' is not available.", fn_name))
    }
    private_env[[fn_name]](...)
  }

  # Assemble the public API surface
  exports <- list()

  if (base::exists("get_all_complexity_metrics", envir = private_env, inherits = FALSE)) {
    exports$get_all_complexity_metrics <- function(loop_function,
                                                   include_continuous = TRUE,
                                                   continuous_vars = c("p", "hipPos", "pelvisPos")) {
      call_private("get_all_complexity_metrics", loop_function, include_continuous, continuous_vars)
    }
  }

  if (base::exists("calculate_complexity_single", envir = private_env, inherits = FALSE)) {
    exports$calculate_complexity_single <- function(participant, trial, allGaitParams,
                                                    outlier_col_names = c("outlierSteps"),
                                                    include_continuous = TRUE,
                                                    continuous_vars = c("p", "hipPos", "pelvisPos"),
                                                    debug = TRUE) {
      call_private("calculate_complexity_single", participant, trial, allGaitParams,
                   outlier_col_names, include_continuous, continuous_vars, debug)
    }
  }

  if (base::exists("calc_complexity_for_loop", envir = private_env, inherits = FALSE)) {
    exports$calc_complexity_for_loop <- function(participant, trial, ...) {
      call_private("calc_complexity_for_loop", participant, trial, ...)
    }
  }

  # Materialize exports back into the feature environment so the loader
  # publishes the intended API surface (and nothing else).
  for (name in names(exports)) {
    base::assign(name, exports[[name]], envir = feature_env)
  }

  exports
})
