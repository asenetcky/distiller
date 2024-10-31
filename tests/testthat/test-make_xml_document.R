test_that("creates an xml document", {
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
  content_group_id <- "AS-HOSP"
  mcn <- "1234-1234-1234-1234-1234"
  jurisdiction_code <- "two_letter_code"
  state_fips_code <- "1234"
  submitter_email <- "submitter@email.com"
  submitter_name <- "Submitter Name"
  submitter_title <- "Submitter Title"

  expect_no_condition(
  doc <- make_document(
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
  expect_s3_class(doc, "xml_document")
})
