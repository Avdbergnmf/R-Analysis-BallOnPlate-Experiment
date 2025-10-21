#' Complexity Module Export Management
#'
#' Collects the functions defined across the complexity feature files and
#' exposes them through the `complexity` object returned by the module loader.

complexity <- local({
  feature_env <- parent.env(environment())

  # Functions injected by the loader that we don't want to export explicitly.
  reserved_names <- c("create_logger", "create_module_logger", "complexity")

  export_names <- setdiff(base::ls(envir = feature_env, all.names = TRUE), reserved_names)
  exports <- list()

  for (name in export_names) {
    obj <- base::get(name, envir = feature_env, inherits = FALSE)
    exports[[name]] <- obj
  }

  # Ensure the exported symbols remain available in the feature environment.
  for (name in names(exports)) {
    base::assign(name, exports[[name]], envir = feature_env)
  }

  exports
})
