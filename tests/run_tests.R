find_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (
      file.exists(file.path(path, "project_support.R")) &&
      dir.exists(file.path(path, "R_functions"))
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
setwd(project_root)

testthat::test_dir(
  "tests",
  reporter = "summary",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)
