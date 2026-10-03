test_that("extract_game_moves extracts moves in order", {
  openings <- c(
    "aa;bb;cc",
    "dd;ee;ff"
  )

  observed <- extract_game_moves(
    openings,
    n_moves = 3
  )

  expected <- matrix(
    c(
      "aa", "dd",
      "bb", "ee",
      "cc", "ff"
    ),
    nrow = 2,
    ncol = 3
  )

  expect_equal(
    observed,
    expected
  )
})


test_that("extract_game_moves can construct cumulative opening prefixes", {
  observed <- extract_game_moves(
    "aa;bb;cc",
    n_moves = 3,
    cumulative = TRUE
  )

  expect_equal(
    as.character(observed[1, ]),
    c("aa", "aabb", "aabbcc")
  )
})


test_that("extract_game_nodes creates color-labelled cumulative nodes", {
  observed <- extract_game_nodes(
    "bb;aa;cc",
    n_moves = 3
  )

  expect_equal(
    as.character(observed[1, ]),
    c("Bbb", "BbbWaa", "BbbBccWaa")
  )
})


test_that("sgf_to_korschelt converts basic board coordinates", {
  expect_equal(
    sgf_to_korschelt(c("aa", "ss")),
    c("A19", "T1")
  )

  expect_equal(
    sgf_to_korschelt("aa;ss;"),
    "A19,T1"
  )
})
