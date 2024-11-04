test_that("check_data returns proper exit status", {
  data_min_vars <-
    mtcars |>
    dplyr::rename(
      month = mpg,
      agegroup = cyl,
      county = disp,
      ethnicity = hp,
      health_outcome_id = drat,
      monthly_count = wt,
      race = qsec,
      sex = vs,
      year = am
    ) |>
    dplyr::select(-c(gear, carb))

  data_add_vars <-
    data_min_vars |>
    dplyr::mutate(
      fire_count = 1,
      nonfire_count = 2,
      unknown_count = 3
    )

  expect_equal(check_data(data_min_vars, "AS-HOSP")$code, 0)
  expect_equal(check_data(data_min_vars, "AS-ED")$code, 0)
  expect_equal(check_data(data_add_vars, "CO-HOSP")$code, 0)
  expect_equal(check_data(data_add_vars, "CO-ED")$code, 0)
  expect_equal(check_data(mtcars, "AS-HOSP")$code, 1)
  expect_equal(check_data(mtcars, "AS-ED")$code, 1)
  expect_equal(check_data(mtcars, "CO-HOSP")$code, 1)
  expect_equal(check_data(mtcars, "CO-ED")$code, 1)
  expect_equal(check_data(data_min_vars, "CO-HOSP")$code, 1)
  expect_equal(check_data(data_min_vars, "CO-ED")$code, 1)
})

test_that("check_content_group_id returns proper exit status", {
  expect_equal(check_content_group_id("AS-ED")$code, 0)
  expect_equal(check_content_group_id("AS-HOSP")$code, 0)
  expect_equal(check_content_group_id("MI-HOSP")$code, 0)
  expect_equal(check_content_group_id("CO-ED")$code, 0)
  expect_equal(check_content_group_id("CO-HOSP")$code, 0)
  expect_equal(check_content_group_id("HEAT-ED")$code, 0)
  expect_equal(check_content_group_id("heat-ed")$code, 1)
  expect_equal(check_content_group_id("huh")$code, 1)
  expect_equal(check_content_group_id(123)$code, 1)
  expect_equal(check_content_group_id(NULL)$code, 1)
  expect_equal(check_content_group_id(NA)$code, 1)
  expect_equal(check_content_group_id(NA_character_)$code, 1)
})

test_that("check_mcn returns proper exit status", {
  expect_equal(check_mcn("12345678-1234-1234-1234-123456789012")$code, 0)
  expect_equal(check_mcn("abcdefgh-abcd-abcd-abcd-abcdefghijkl")$code, 0)
  expect_equal(check_mcn("123abc12-a1b2-2b1a-defg-abcd1234abcd")$code, 0)
  expect_equal(check_mcn(123)$code, 1)
  expect_equal(check_mcn(NULL)$code, 1)
  expect_equal(check_mcn(NA)$code, 1)
  expect_equal(check_mcn(NA_character_)$code, 1)
  expect_equal(check_mcn("")$code, 2)
  expect_equal(check_mcn("123a1b2-2b1a-defg-abcd1234abcd")$code, 2)
  expect_equal(check_mcn("123456789012345678901234567890123456")$code, 2)
})

test_that("check_jurisdiction_code returns proper exit status", {
  expect_equal(check_jurisdiction_code("AL")$code, 0)
  expect_equal(check_jurisdiction_code("AK")$code, 0)
  expect_equal(check_jurisdiction_code("AZ")$code, 0)
  expect_equal(check_jurisdiction_code(12)$code, 1)
  expect_equal(check_jurisdiction_code(NULL)$code, 1)
  expect_equal(check_jurisdiction_code(NA)$code, 1)
  expect_equal(check_jurisdiction_code(NA_character_)$code, 1)
  expect_equal(check_jurisdiction_code("two_letter_code")$code, 2)
  expect_equal(check_jurisdiction_code("12")$code, 2)
})

test_that("check_state_fips_code returns proper exit status", {
  expect_equal(check_state_fips_code("01")$code, 0)
  expect_equal(check_state_fips_code("02")$code, 0)
  expect_equal(check_state_fips_code("03")$code, 0)
  expect_equal(check_state_fips_code(12)$code, 1)
  expect_equal(check_state_fips_code(NULL)$code, 1)
  expect_equal(check_state_fips_code(NA)$code, 1)
  expect_equal(check_state_fips_code(NA_character_)$code, 1)
  expect_equal(check_state_fips_code("123")$code, 2)
  expect_equal(check_state_fips_code("1")$code, 2)
})

test_that("check_submitter_email returns proper exit status", {
 expect_equal(check_submitter_email("email@email.com")$code, 0)
 expect_equal(check_submitter_email("somethinglong123@email.com")$code, 0)
 expect_equal(check_submitter_email("a@email.com")$code, 0)
 expect_equal(check_submitter_email(1)$code, 1)
 expect_equal(check_submitter_email(NULL)$code, 1)
 expect_equal(check_submitter_email(NA)$code, 1)
 expect_equal(check_submitter_email(NA_character_)$code, 1)
 expect_equal(check_submitter_email("email.com")$code, 2)
 expect_equal(check_submitter_email("123")$code, 2)
})

test_that("check_submitter_name returns proper exit status", {
  expect_equal(check_submitter_name("First Last")$code, 0)
  expect_equal(check_submitter_name(1)$code, 1)
  expect_equal(check_submitter_name(NULL)$code, 1)
  expect_equal(check_submitter_name(NA)$code, 1)
  expect_equal(check_submitter_name(NA_character_)$code, 1)
  expect_equal(check_submitter_name("First Middle Last")$code, 2)
  expect_equal(check_submitter_name("First")$code, 2)
  expect_equal(check_submitter_name("First Middle Last Name")$code, 2)
})


test_that("check_submitter_title returns proper exit status", {
  expect_equal(check_submitter_title("Title")$code, 0)
  expect_equal(check_submitter_title(1)$code, 1)
  expect_equal(check_submitter_title(NULL)$code, 1)
  expect_equal(check_submitter_title(NA)$code, 1)
  expect_equal(check_submitter_title(NA_character_)$code, 1)
  expect_equal(check_submitter_title("")$code, 2)
})


test_that("check_* functions return a message", {
  data_min_vars <-
    mtcars |>
    dplyr::rename(
      month = mpg,
      agegroup = cyl,
      county = disp,
      ethnicity = hp,
      health_outcome_id = drat,
      monthly_count = wt,
      race = qsec,
      sex = vs,
      year = am
    ) |>
    dplyr::select(-c(gear, carb))

  content_group_id <- "AS-HOSP"
  mcn <- "1234-1234-1234-1234-1234"
  jurisdiction_code <- "two_letter_code"
  state_fips_code <- "1234"
  submitter_email <- "submitter@email.com"
  submitter_name <- "Submitter Name"
  submitter_title <- "Submitter Title"

  expect_equal(check_data(
    data_min_vars, content_group_id)$message,
    "Success: data"
  )
  expect_equal(check_content_group_id(
    content_group_id)$message,
    "Success: content_group_id"
  )
  expect_equal(
    check_mcn(mcn)$message,
    "Warning: mcn may not have correct format\nTroublemakers: length, format"
  )
  expect_equal(
    check_jurisdiction_code(jurisdiction_code)$message,
    "Warning: jurisdiction_code may not have correct format\nTroublemakers: length, format"
  )
  expect_equal(
    check_state_fips_code(state_fips_code)$message,
    "Warning: state_fips_code may not have correct format\nTroublemakers: length, format"
  )
  expect_equal(
    check_submitter_email(submitter_email)$message,
    "Success: submitter_email"
  )
  expect_equal(
    check_submitter_name(submitter_name)$message,
    "Success: submitter_name"
  )
  expect_equal(
    check_submitter_title(submitter_title)$message,
    "Success: submitter_title"
  )
})
