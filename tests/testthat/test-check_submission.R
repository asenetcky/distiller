test_that("check_submission runs without error", {
  data <-
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

  # And your metadata
  content_group_id <- "AS-HOSP"
  mcn <- "1234-1234-1234-1234-1234"
  jurisdiction_code <- "two_letter_code"
  state_fips_code <- "1234"
  submitter_email <- "submitter@email.com"
  submitter_name <- "Submitter Name"
  submitter_title <- "Submitter Title"

  expect_no_error(
    check_submission(
      data,
      content_group_id,
      mcn,
      jurisdiction_code,
      state_fips_code,
      submitter_email,
      submitter_name,
      submitter_title
    )
  )
})
