# ============================================================================
# Run tests
#
# This script:
#   1. locates the project root
#   2. loads R/functions/restore_environment.R
#   3. restores the package environment recorded in renv.lock
#   4. runs the testthat suite in tests/
#
# Run with:
#
#   Rscript tests/run_tests.R
#
# ============================================================================


# ============================================================================
# Clear workspace
# ============================================================================

rm(list = ls())


# ============================================================================
# Locate project root
# ============================================================================

find_project_root <- function(start = getwd()) {

  path <- normalizePath(
    start,
    winslash = "/",
    mustWork = TRUE
  )

  repeat {

    restore_helper <- file.path(
      path,
      "R",
      "functions",
      "restore_environment.R"
    )

    if (
      file.exists(
        file.path(
          path,
          "renv.lock"
        )
      ) &&
      file.exists(
        restore_helper
      ) &&
      dir.exists(
        file.path(
          path,
          "tests"
        )
      )
    ) {

      return(path)
    }

    parent <- dirname(path)

    if (identical(
      parent,
      path
    )) {

      stop(
        "Could not locate project root from: ",
        start
      )
    }

    path <- parent
  }
}


project_root <- find_project_root()

setwd(
  project_root
)


# ============================================================================
# Restore package environment
# ============================================================================

restore_helper <- file.path(
  project_root,
  "R",
  "functions",
  "restore_environment.R"
)

source(
  restore_helper,
  local = FALSE
)

if (!exists(
  "restore_environment",
  mode = "function",
  inherits = TRUE
)) {

  stop(
    "R/functions/restore_environment.R did not define ",
    "restore_environment()."
  )
}

restore_environment(
  project_root
)


# ============================================================================
# Run tests
# ============================================================================

if (!requireNamespace(
  "testthat",
  quietly = TRUE
)) {

  stop(
    "testthat is not installed after restoring renv.lock. ",
    "Confirm that testthat is recorded in renv.lock."
  )
}

testthat::test_dir(
  "tests",
  reporter = "summary",
  stop_on_failure = TRUE,
  stop_on_warning = FALSE
)
