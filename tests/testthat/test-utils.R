test_that("right type returned", {
  expect_equal(
    parse_content_group_id("AS-HOSP"),
    "hosp"
  )
  expect_equal(
    parse_content_group_id("AS-ED"),
    "ed"
  )
})

test_that("errors correctly thrown", {
  expect_error(parse_content_group_id("TESTING123"))
  expect_error(parse_content_group_id(123))
  expect_error(parse_content_group_id(lubridate::now()))

})
