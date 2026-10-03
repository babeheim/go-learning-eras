test_that("move_to_start and move_to_end reorder vectors", {
  x <- letters[1:4]

  expect_equal(
    move_to_start(x, 3),
    c("c", "a", "b", "d")
  )

  expect_equal(
    move_to_end(x, 2),
    c("a", "c", "d", "b")
  )
})


test_that("move_to_start and move_to_end reject invalid indices", {
  expect_error(
    move_to_start(1:3, 0),
    "Index out of bounds"
  )

  expect_error(
    move_to_end(1:3, 4),
    "Index out of bounds"
  )
})


test_that("HPDI returns sensible intervals", {
  interval <- HPDI(
    rep(3, 100),
    prob = 0.89
  )

  expect_length(
    interval,
    2
  )

  expect_equal(
    unname(interval),
    c(3, 3)
  )

  expect_error(
    HPDI(1:10, prob = 1),
    "prob must define an interval"
  )
})


test_that("dir_init creates an empty directory", {
  path <- tempfile("dir-init-")

  dir.create(
    path,
    recursive = TRUE
  )

  writeLines(
    "temporary file",
    file.path(path, "old-file.txt")
  )

  dir.create(
    file.path(path, "old-subdirectory")
  )

  result <- dir_init(path)

  expect_true(
    dir.exists(path)
  )

  expect_length(
    list.files(
      path,
      all.files = TRUE,
      no.. = TRUE
    ),
    0
  )

  expect_equal(
    normalizePath(result, winslash = "/"),
    normalizePath(path, winslash = "/")
  )

  unlink(
    path,
    recursive = TRUE
  )
})
