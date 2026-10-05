# ============================================================================
# Run project
#
# This script:
#   1. loads R/functions/restore_environment.R
#   2. restores the package environment recorded in renv.lock
#   3. creates a timestamped execution log
#   4. records Git, R, renv, package-version, and CmdStan information
#   5. loads project_support.R
#   6. runs the complete analysis workflow
#   7. records tictoc timings
#   8. records sessionInfo()
#   9. records SUCCESS / FAILED status
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
# Load environment-restore helper
# ============================================================================

restore_environment <- function(project = ".") {

  project <- normalizePath(
    project,
    winslash = "/",
    mustWork = TRUE
  )

  lockfile <- file.path(
    project,
    "renv.lock"
  )

  activate_file <- file.path(
    project,
    "renv",
    "activate.R"
  )

  if (!file.exists(lockfile)) {
    stop(
      "renv.lock was not found: ",
      lockfile
    )
  }

  if (!file.exists(activate_file)) {
    stop(
      "renv activation script was not found: ",
      activate_file
    )
  }

  old_wd <- getwd()

  on.exit(
    setwd(old_wd),
    add = TRUE
  )

  setwd(project)

  # Bootstrap and activate the project's own renv environment.
  #
  # renv/activate.R can bootstrap renv itself on a fresh machine,
  # so renv does not need to be installed globally beforehand.
  source(
    activate_file,
    local = .GlobalEnv
  )

  if (!requireNamespace(
    "renv",
    quietly = TRUE
  )) {
    stop(
      "renv could not be bootstrapped from renv/activate.R."
    )
  }

  # Restore exactly the package versions recorded in renv.lock.
  renv::restore(
    project = project,
    lockfile = lockfile,
    prompt = FALSE,
    retry = FALSE
  )

  # Explicitly ensure that this running R process is using the
  # restored project library.
  renv::load(
    project = project,
    quiet = TRUE
  )

  invisible(TRUE)
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
    # Restore package environment
    # ========================================================================

    section_header(
      "RENV RESTORE"
    )

    cat(
      "Restoring package environment from renv.lock...\n"
    )

    restore_environment(
      project_root
    )

    cat(
      "\nrenv restore completed.\n"
    )


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
    # R environment
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

    package_table$version_match <- mapply(
      function(locked, installed) {

        if (is.na(locked) || is.na(installed)) {
          return(FALSE)
        }

        identical(
          package_version(locked),
          package_version(installed)
        )
      },
      package_table$locked_version,
      package_table$installed_version
    )

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
    # CmdStan environment
    # ========================================================================

    section_header(
      "CMDSTAN ENVIRONMENT"
    )

    cmdstanr_version <- if (requireNamespace(
      "cmdstanr",
      quietly = TRUE
    )) {

      as.character(
        packageVersion(
          "cmdstanr"
        )
      )

    } else {

      NA_character_
    }

    cmdstan_version <- tryCatch(
      {
        as.character(
          cmdstanr::cmdstan_version()
        )
      },
      error = function(e) {
        NA_character_
      }
    )

    cat(
      "cmdstanr version: ",
      if (is.na(
        cmdstanr_version
      )) {
        "not available"
      } else {
        cmdstanr_version
      },
      "\n",
      "CmdStan version:  ",
      if (is.na(
        cmdstan_version
      )) {
        "not available"
      } else {
        cmdstan_version
      },
      "\n",
      sep = ""
    )


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

    dir_init(
      "cached"
    )

    cat(
      "Cached directory initialized.\n"
    )

    # ============================================================================
    # Analysis
    # ============================================================================

    section_header("ANALYSIS")


    # ------------------------------------------------------------------------
    # Discover analysis scripts
    # ------------------------------------------------------------------------

    analysis_dir <- file.path(
      project_root,
      "R"
    )

    analysis_scripts <- list.files(
      path = analysis_dir,
      pattern = "^[0-9]+_.*\\.R$",
      full.names = TRUE
    )

    if (length(analysis_scripts) == 0L) {
      stop(
        "No numbered analysis scripts were found in ",
        analysis_dir,
        "."
      )
    }


    # ------------------------------------------------------------------------
    # Order analysis scripts
    # ------------------------------------------------------------------------

    analysis_filenames <- basename(
      analysis_scripts
    )

    analysis_numbers <- as.integer(
      sub(
        "^([0-9]+)_.*$",
        "\\1",
        analysis_filenames
      )
    )

    analysis_order <- order(
      analysis_numbers,
      analysis_filenames
    )

    analysis_scripts <- analysis_scripts[
      analysis_order
    ]

    analysis_filenames <- analysis_filenames[
      analysis_order
    ]

    analysis_numbers <- analysis_numbers[
      analysis_order
    ]


    # ------------------------------------------------------------------------
    # Verify analysis numbering
    # ------------------------------------------------------------------------

    if (anyDuplicated(
      analysis_numbers
    )) {

      duplicated_numbers <- unique(
        analysis_numbers[
          duplicated(
            analysis_numbers
          ) |
            duplicated(
              analysis_numbers,
              fromLast = TRUE
            )
        ]
      )

      stop(
        "Duplicate analysis-script numbers detected: ",
        paste(
          duplicated_numbers,
          collapse = ", "
        )
      )
    }


    # ------------------------------------------------------------------------
    # Report workflow
    # ------------------------------------------------------------------------

    cat(
      "Analysis scripts found:\n\n"
    )

    for (i in seq_along(
      analysis_scripts
    )) {

      cat(
        sprintf(
          "  %02d. %s\n",
          i,
          analysis_filenames[i]
        )
      )
    }

    cat("\n")


    # ------------------------------------------------------------------------
    # Run analysis scripts
    # ------------------------------------------------------------------------

    for (i in seq_along(
      analysis_scripts
    )) {

      script_file <- analysis_scripts[i]

      script_name <- tools::file_path_sans_ext(
        analysis_filenames[i]
      )

      script_label <- sub(
        "^[0-9]+_",
        "",
        script_name
      )

      script_label <- gsub(
        "_",
        " ",
        script_label,
        fixed = TRUE
      )

      timing_results[[script_name]] <- run_script(
        file = script_file,
        label = script_label
      )
    }

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
