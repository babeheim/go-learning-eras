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
