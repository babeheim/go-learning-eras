# ============================================================================
# Run project
#
# This script:
#   1. restores the package environment recorded in renv.lock
#   2. creates a timestamped execution log
#   3. records Git, R, renv, and package-version information
#   4. loads project_support.R
#   5. runs the complete analysis workflow
#   6. records tictoc timings
#   7. records sessionInfo()
#   8. records SUCCESS / FAILED status
#
# Run from the project root with:
#
#   Rscript run_project.R
#
# ============================================================================


# ============================================================================
# Clear workspace
# ============================================================================

rm(list = ls())


# ============================================================================
# Basic project setup
# ============================================================================

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

run_started <- Sys.time()

timestamp <- format(
  run_started,
  "%Y-%m-%d_%H-%M-%S"
)

log_dir <- file.path(
  project_root,
  "logs"
)

dir.create(
  log_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

log_file <- file.path(
  log_dir,
  paste0(
    "run-",
    timestamp,
    ".log"
  )
)

latest_log <- file.path(
  log_dir,
  "latest.log"
)


# ============================================================================
# Bootstrap renv
# ============================================================================

if (!requireNamespace(
  "renv",
  quietly = TRUE
)) {

  install.packages(
    "renv",
    repos = "https://cloud.r-project.org"
  )
}


# ============================================================================
# Logging helpers
# ============================================================================

section_header <- function(title) {

  cat(
    "\n",
    "============================================================\n",
    title,
    "\n",
    "============================================================\n",
    sep = ""
  )
}


`%||%` <- function(x, y) {

  if (is.null(x)) {
    y
  } else {
    x
  }
}


git_value <- function(args) {

  tryCatch(
    {
      out <- system2(
        command = "git",
        args = args,
        stdout = TRUE,
        stderr = FALSE
      )

      if (length(out) == 0L) {
        return(NA_character_)
      }

      paste(
        out,
        collapse = " "
      )
    },
    error = function(e) {
      NA_character_
    }
  )
}


run_script <- function(file, label = file) {

  cat(
    "\n",
    "------------------------------------------------------------\n",
    "START: ",
    label,
    "\n",
    "SCRIPT: ",
    file,
    "\n",
    "------------------------------------------------------------\n",
    sep = ""
  )

  started <- Sys.time()

  tictoc::tic(
    label
  )

  source(
    file,
    local = FALSE,
    echo = FALSE
  )

  tictoc::toc(
    log = TRUE
  )

  finished <- Sys.time()

  elapsed <- as.numeric(
    difftime(
      finished,
      started,
      units = "secs"
    )
  )

  cat(
    "COMPLETED: ",
    label,
    " (",
    sprintf(
      "%.1f s",
      elapsed
    ),
    ")\n",
    sep = ""
  )

  invisible(
    data.frame(
      label = label,
      file = file,
      started = as.character(started),
      finished = as.character(finished),
      elapsed_seconds = elapsed,
      stringsAsFactors = FALSE
    )
  )
}


# ============================================================================
# Start logfile
# ============================================================================

log_connection <- file(
  log_file,
  open = "wt"
)

sink(
  log_connection,
  split = TRUE
)

sink(
  log_connection,
  type = "message"
)

logging_active <- TRUE


close_log <- function() {

  if (!logging_active) {
    return(
      invisible(NULL)
    )
  }

  try(
    sink(
      type = "message"
    ),
    silent = TRUE
  )

  try(
    sink(),
    silent = TRUE
  )

  try(
    close(
      log_connection
    ),
    silent = TRUE
  )

  logging_active <<- FALSE

  invisible(NULL)
}


# ============================================================================
# Run bookkeeping
# ============================================================================

run_success <- FALSE

run_error <- NULL

timing_results <- list()


# ============================================================================
# Main project execution
# ============================================================================

tryCatch(

  {

    # ========================================================================
    # Run metadata
    # ========================================================================

    section_header(
      "PROJECT RUN"
    )

    cat(
      "Project root:     ",
      project_root,
      "\n",
      "Started:          ",
      format(
        run_started,
        "%Y-%m-%d %H:%M:%S"
      ),
      "\n",
      "Machine:          ",
      Sys.info()[["nodename"]],
      "\n",
      "Operating system: ",
      Sys.info()[["sysname"]],
      " ",
      Sys.info()[["release"]],
      "\n",
      "Architecture:     ",
      R.version$arch,
      "\n",
      sep = ""
    )


    # ========================================================================
    # Git metadata
    # ========================================================================

    section_header(
      "GIT STATUS"
    )

    git_commit <- git_value(
      c(
        "rev-parse",
        "HEAD"
      )
    )

    git_branch <- git_value(
      c(
        "branch",
        "--show-current"
      )
    )

    git_remote <- git_value(
      c(
        "config",
        "--get",
        "remote.origin.url"
      )
    )

    git_status <- tryCatch(
      system2(
        command = "git",
        args = c(
          "status",
          "--porcelain"
        ),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(e) {
        character(0)
      }
    )

    git_dirty <- length(
      git_status
    ) > 0L

    cat(
      "Branch:     ",
      git_branch,
      "\n",
      "Commit:     ",
      git_commit,
      "\n",
      "Remote:     ",
      git_remote,
      "\n",
      "Dirty tree: ",
      if (git_dirty) {
        "YES"
      } else {
        "NO"
      },
      "\n",
      sep = ""
    )

    if (git_dirty) {

      cat(
        "\nUncommitted changes:\n"
      )

      cat(
        paste(
          git_status,
          collapse = "\n"
        ),
        "\n"
      )
    }


    # ========================================================================
    # R environment before restoration
    # ========================================================================

    section_header(
      "R ENVIRONMENT"
    )

    cat(
      "Current R version:    ",
      R.version.string,
      "\n",
      "Platform:             ",
      R.version$platform,
      "\n",
      "renv version:         ",
      as.character(
        packageVersion(
          "renv"
        )
      ),
      "\n",
      sep = ""
    )


    # ========================================================================
    # Verify renv.lock exists
    # ========================================================================

    if (!file.exists(
      file.path(
        project_root,
        "renv.lock"
      )
    )) {

      stop(
        "renv.lock was not found in the project root."
      )
    }


    # ========================================================================
    # Read lockfile metadata
    # ========================================================================

    lock <- renv::lockfile_read(
      file.path(
        project_root,
        "renv.lock"
      )
    )

    locked_r_version <- lock$R$Version %||% NA_character_

    locked_renv_version <- lock$Packages$renv$Version %||%
      lock$renv$Version %||%
      NA_character_

    cat(
      "Locked R version:     ",
      locked_r_version,
      "\n",
      "Locked renv version:  ",
      locked_renv_version,
      "\n",
      sep = ""
    )


    # ========================================================================
    # Restore package environment
    # ========================================================================

    section_header(
      "RENV RESTORE"
    )

    cat(
      "Restoring package environment from renv.lock...\n"
    )

    renv::restore(
      project = project_root,
      prompt = FALSE,
      retry = FALSE
    )

    cat(
      "\nrenv restore completed.\n"
    )


    # ========================================================================
    # Check renv status
    # ========================================================================

    section_header(
      "RENV STATUS"
    )

    renv_status <- renv::status(
      project = project_root
    )

    cat(
      "\nSynchronized: ",
      if (isTRUE(
        renv_status$synchronized
      )) {
        "YES"
      } else {
        "NO"
      },
      "\n",
      sep = ""
    )


    # ========================================================================
    # Package environment
    # ========================================================================

    section_header(
      "PACKAGE ENVIRONMENT"
    )

    lock <- renv::lockfile_read(
      file.path(
        project_root,
        "renv.lock"
      )
    )

    packages <- lock$Packages

    package_names <- names(
      packages
    )

    package_table <- data.frame(
      package = package_names,

      locked_version = vapply(
        packages,
        function(x) {
          x$Version %||% NA_character_
        },
        character(1)
      ),

      installed_version = vapply(
        package_names,
        function(package) {

          if (requireNamespace(
            package,
            quietly = TRUE
          )) {

            as.character(
              packageVersion(
                package
              )
            )

          } else {

            NA_character_
          }
        },
        character(1)
      ),

      source = vapply(
        packages,
        function(x) {
          x$Source %||% NA_character_
        },
        character(1)
      ),

      repository = vapply(
        packages,
        function(x) {
          x$Repository %||% NA_character_
        },
        character(1)
      ),

      stringsAsFactors = FALSE
    )

    package_table <- package_table[
      order(
        package_table$package
      ),
      ,
      drop = FALSE
    ]

    package_table$version_match <-
      package_table$locked_version ==
      package_table$installed_version

    print(
      package_table,
      row.names = FALSE
    )


    # ========================================================================
    # Verify package versions
    # ========================================================================

    version_mismatches <- package_table[
      is.na(
        package_table$installed_version
      ) |
        !package_table$version_match,
      ,
      drop = FALSE
    ]

    if (nrow(
      version_mismatches
    ) > 0L) {

      cat(
        "\nWARNING: package-version mismatches detected:\n\n"
      )

      print(
        version_mismatches,
        row.names = FALSE
      )

    } else {

      cat(
        "\nAll locked package versions are installed as expected.\n"
      )
    }


    # ========================================================================
    # Load project support
    # ========================================================================

    section_header(
      "PROJECT SUPPORT"
    )

    cat(
      "Loading project_support.R...\n"
    )

    source(
      "project_support.R"
    )

    cat(
      "project_support.R loaded successfully.\n"
    )


    # ========================================================================
    # Initialize output directories
    # ========================================================================

    section_header(
      "PROJECT INITIALIZATION"
    )

    cat(
      "Initializing figures directory...\n"
    )

    dir_init(
      "figures"
    )

    cat(
      "Figures directory initialized.\n"
    )


    # ========================================================================
    # Analysis
    # ========================================================================

    section_header(
      "ANALYSIS"
    )


    # ------------------------------------------------------------------------
    # Plot openings
    # ------------------------------------------------------------------------

    timing_results[["plot_openings"]] <- run_script(
      file = "R_scripts/plot_openings.R",
      label = "plot openings"
    )


    # ------------------------------------------------------------------------
    # Plot opening trees
    # ------------------------------------------------------------------------

    timing_results[["plot_opening_trees"]] <- run_script(
      file = "R_scripts/plot_opening_trees.R",
      label = "plot opening trees"
    )


    # ------------------------------------------------------------------------
    # Plot database coverage
    # ------------------------------------------------------------------------

    timing_results[["plot_database_coverage"]] <- run_script(
      file = "R_scripts/plot_database_coverage.R",
      label = "plot database coverage"
    )


    # ------------------------------------------------------------------------
    # Calculate game distances
    # ------------------------------------------------------------------------

    timing_results[["calc_game_distances"]] <- run_script(
      file = "R_scripts/calc_game_distances.R",
      label = "calculate game distances"
    )


    # ------------------------------------------------------------------------
    # Calculate match networks
    # ------------------------------------------------------------------------

    timing_results[["calc_match_networks"]] <- run_script(
      file = "R_scripts/calc_match_networks.R",
      label = "calculate match networks"
    )


    # ------------------------------------------------------------------------
    # Analyze opening diversity
    # ------------------------------------------------------------------------

    timing_results[["analyze_opening_diversity"]] <- run_script(
      file = "R_scripts/analyze_opening_diversity.R",
      label = "analyze opening diversity"
    )


    # ------------------------------------------------------------------------
    # Analyze opening diversity CN
    # ------------------------------------------------------------------------

    timing_results[["analyze_opening_diversity_CN"]] <- run_script(
      file = "R_scripts/analyze_opening_diversity_CN.R",
      label = "analyze opening diversity in China"
    )


    # ------------------------------------------------------------------------
    # Analyze opening diversity JP
    # ------------------------------------------------------------------------

    timing_results[["analyze_opening_diversity_JP"]] <- run_script(
      file = "R_scripts/analyze_opening_diversity_JP.R",
      label = "analyze opening diversity in Japan"
    )


    # ------------------------------------------------------------------------
    # Analyze opening diversity KR
    # ------------------------------------------------------------------------

    timing_results[["analyze_opening_diversity_KR"]] <- run_script(
      file = "R_scripts/analyze_opening_diversity_KR.R",
      label = "analyze opening diversity in South Korea"
    )


    # ------------------------------------------------------------------------
    # Analyze speed evolution
    # ------------------------------------------------------------------------

    timing_results[["analyze_speed_evolution"]] <- run_script(
      file = "R_scripts/analyze_speed_evolution.R",
      label = "analyze speed evolution"
    )


    # ========================================================================
    # Timing summary
    # ========================================================================

    section_header(
      "TIMING SUMMARY"
    )

    timing_table <- do.call(
      rbind,
      timing_results
    )

    rownames(
      timing_table
    ) <- NULL

    print(
      timing_table[
        ,
        c(
          "label",
          "elapsed_seconds"
        )
      ],
      row.names = FALSE
    )

    cat(
      "\n",
      "tictoc log:\n",
      sep = ""
    )

    print(
      tictoc::tic.log(
        format = TRUE
      )
    )


    # ========================================================================
    # Session information
    # ========================================================================

    section_header(
      "SESSION INFO"
    )

    print(
      sessionInfo()
    )


    # ========================================================================
    # Mark run successful
    # ========================================================================

    run_success <- TRUE

  },

  error = function(e) {

    run_error <<- e

    section_header(
      "ERROR"
    )

    cat(
      "Project execution failed.\n\n",
      "Message:\n",
      conditionMessage(
        e
      ),
      "\n",
      sep = ""
    )

    calls <- sys.calls()

    if (length(
      calls
    ) > 0L) {

      cat(
        "\nCall stack:\n"
      )

      print(
        calls
      )
    }
  }
)


# ============================================================================
# Final run summary
# ============================================================================

run_finished <- Sys.time()

run_elapsed <- as.numeric(
  difftime(
    run_finished,
    run_started,
    units = "secs"
  )
)

section_header(
  "RUN SUMMARY"
)

cat(
  "Status:   ",
  if (run_success) {
    "SUCCESS"
  } else {
    "FAILED"
  },
  "\n",
  "Started:  ",
  format(
    run_started,
    "%Y-%m-%d %H:%M:%S"
  ),
  "\n",
  "Finished: ",
  format(
    run_finished,
    "%Y-%m-%d %H:%M:%S"
  ),
  "\n",
  "Elapsed:  ",
  sprintf(
    "%.1f s",
    run_elapsed
  ),
  "\n",
  "Log file: ",
  log_file,
  "\n",
  sep = ""
)

if (!run_success &&
    !is.null(
      run_error
    )) {

  cat(
    "Error:    ",
    conditionMessage(
      run_error
    ),
    "\n",
    sep = ""
  )
}


# ============================================================================
# Close logfile
# ============================================================================

close_log()


# ============================================================================
# Maintain logs/latest.log
# ============================================================================

file.copy(
  from = log_file,
  to = latest_log,
  overwrite = TRUE
)


# ============================================================================
# Console completion message
# ============================================================================

cat(
  "\n",
  "============================================================\n",
  if (run_success) {
    "PROJECT COMPLETED SUCCESSFULLY"
  } else {
    "PROJECT FAILED"
  },
  "\n",
  "============================================================\n",
  "Elapsed: ",
  sprintf(
    "%.1f s",
    run_elapsed
  ),
  "\n",
  "Log:     ",
  log_file,
  "\n",
  sep = ""
)


# ============================================================================
# Exit status
# ============================================================================

if (!run_success) {

  quit(
    save = "no",
    status = 1L
  )
}

quit(
  save = "no",
  status = 0L
)
