test_that("ethnicity collapses correctly", {
  expect_equal(
    collapse_ethnicity("Hispanic"),
    "H"
  )
  expect_equal(
    collapse_ethnicity("Non-Hispanic"),
    "NH"
  )
  expect_equal(
    collapse_ethnicity("Unknown"),
    "U"
  )
  expect_equal(
    collapse_ethnicity("his-panic"),
    "H"
  )
  expect_equal(
    collapse_ethnicity("nonhispanic"),
    "NH"
  )
  expect_equal(
    collapse_ethnicity("hispanic"),
    "H"
  )
  expect_equal(
    collapse_ethnicity(NA, na_is_unknown = TRUE),
    "U"
  )
  expect_equal(
    collapse_ethnicity(NA, na_is_unknown = FALSE),
    NA_character_
  )
})

test_that("race collapses correctly", {
  expect_equal(
    collapse_race("White"),
    "W"
  )
  expect_equal(
    collapse_race("white"),
    "W"
  )
  expect_equal(
    collapse_race("Wh-iT!e"),
    "W"
  )
  expect_equal(
    collapse_race("Black"),
    "B"
  )
  expect_equal(
    collapse_race("Asian"),
    "O"
  )
  expect_equal(
    collapse_race("Other"),
    "O"
  )
  expect_equal(
    collapse_race(NA, na_is_unknown = TRUE),
    "U"
  )
  expect_equal(
    collapse_race(NA, na_is_unknown = FALSE),
    NA_character_
  )
})
