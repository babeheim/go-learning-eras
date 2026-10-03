# Shared test setup ---------------------------------------------------------
#
# These tests are intentionally independent of project_support.R so that unit
# tests can exercise the project's own functions without attaching the full
# analysis stack (e.g. CmdStan). CI separately smoke-tests project_support.R.

find_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (
      file.exists(file.path(path, "project_support.R")) &&
      dir.exists(file.path(path, "R"))
    ) {
      return(path)
    }

    parent <- dirname(path)

    if (identical(parent, path)) {
      stop("Could not locate project root from: ", start)
    }

    path <- parent
  }
}

project_root <- find_project_root()

# calc_js_divergence() currently calls JSD() without a namespace, matching the
# analysis environment created by project_support.R.
suppressPackageStartupMessages(
  library(philentropy)
)

source(
  file.path(project_root, "R/functions", "diversity_functions.R"),
  local = FALSE
)

source(
  file.path(project_root, "R/functions", "misc_functions.R"),
  local = FALSE
)

source(
  file.path(project_root, "R/functions", "kaya_functions.R"),
  local = FALSE
)
