test_that("required project entry points are present", {
  required <- c(
    "project_support.R",
    "run_project.R",
    "renv.lock"
  )

  paths <- file.path(
    project_root,
    required
  )

  expect_true(
    all(file.exists(paths)),
    info = paste(
      "Missing:",
      paste(required[!file.exists(paths)], collapse = ", ")
    )
  )
})


test_that("required source directories are present", {
  required <- c(
    "R_functions",
    "R_scripts",
    "data"
  )

  paths <- file.path(
    project_root,
    required
  )

  expect_true(
    all(dir.exists(paths)),
    info = paste(
      "Missing:",
      paste(required[!dir.exists(paths)], collapse = ", ")
    )
  )
})


test_that("required analytical data files are present", {
  required <- c(
    "games.csv",
    "players.csv",
    "eras.csv",
    "move12s.csv",
    "move123s.csv"
  )

  paths <- file.path(
    project_root,
    "data",
    required
  )

  expect_true(
    all(file.exists(paths)),
    info = paste(
      "Missing:",
      paste(required[!file.exists(paths)], collapse = ", ")
    )
  )
})
