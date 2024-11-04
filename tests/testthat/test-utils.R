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

test_that("right health outcome id returned", {
  expect_equal(
    parse_health_outcome_id("AS-HOSP"),
    1
  )
  expect_equal(
    parse_health_outcome_id("AS-ED"),
    1
  )
  expect_equal(
    parse_health_outcome_id("MI-HOSP"),
    2
  )
  expect_equal(
    parse_health_outcome_id("CO-ED"),
    3
  )
 expect_equal(
    parse_health_outcome_id("CO-HOSP"),
    3
  )
})

test_that("errors correctly thrown", {
  expect_error(parse_content_group_id("TESTING123"))
  expect_error(parse_content_group_id(123))
  expect_error(parse_content_group_id(Sys.time()))
  expect_error(parse_health_outcome_id("AS-HOSP-ED"))
})

test_that("months are made worse successfully", {
  expect_equal(
    make_months_worse(1),
    "01"
  )
  expect_equal(
    make_months_worse(9),
    "09"
  )
  expect_equal(
    make_months_worse(10),
    "10"
  )
  expect_equal(
    make_months_worse(12),
    "12"
  )
})


test_that("warnings/errors are thrown for non-month integers", {
  expect_error(make_months_worse("test"))
  expect_error(make_months_worse(Sys.time()))
  expect_error(make_months_worse(NULL))
  expect_error(make_months_worse(Inf))
  expect_error(make_months_worse(123.45))
})
