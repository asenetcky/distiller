test_that("throws errors on improper arguments", {
  expect_error(make_dataset())
  expect_error(make_dataset(.data = "huh", type = "hosp"))
  expect_error(make_dataset(.data = "huh", type = "ed"))
  expect_error(make_dataset(123))
  expect_error(make_dataset(lubridate::now()))

  data_wrong_vars <- mtcars
  data_right_vars <-
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

  expect_error(make_dataset(.data = data_wrong_vars, type = "hosp"))
  expect_error(make_dataset(.data = data_wrong_vars, type = "ed"))
  expect_error(make_dataset(.data = data_right_vars, type = "huh"))
})

test_that("returns XML node", {
   data_right_vars <-
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

  expect_s3_class(
    make_dataset(.data = data_right_vars, type = "hosp"), "xml_node"
  )
  expect_s3_class(
    make_dataset(.data = data_right_vars, type = "ed"), "xml_node"
  )
  expect_s3_class(
    make_dataset(.data = data_right_vars, type = "hosp"), "xml_document"
  )
  expect_s3_class(
    make_dataset(.data = data_right_vars, type = "ed"), "xml_document"
  )
})


test_that("right root element and variable order for the right content type", {
  data_right_vars <-
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


  xml_list_hosp <-
    make_dataset(.data = data_right_vars, type = "hosp") |>
    xml2::as_list()

  xml_list_ed <-
    make_dataset(.data = data_right_vars, type = "ed") |>
    xml2::as_list()

  expect_equal(
    make_dataset(.data = data_right_vars, type = "hosp") |>xml2::xml_name(),
    "Dataset"
  )
  expect_equal(
    make_dataset(.data = data_right_vars, type = "ed") |>  xml2::xml_name(),
    "Dataset"
  )

  expect_equal(
    names(xml_list_hosp$Dataset$Row |> dplyr::as_tibble()),
    as.character(
      c(
        "RowIdentifier",
        "AdmissionMonth",
        "AgeGroup",
        "County",
        "Ethnicity",
        "HealthOutcomeID",
        "MonthlyHosp",
        "Race",
        "Sex",
        "YearAdmitted"
      )
    )
  )
  expect_equal(
    names(xml_list_ed$Dataset$Row |> dplyr::as_tibble()),
    as.character(
      c(
        "RowIdentifier",
        "AgeGroup",
        "County",
        "EdVisitYear",
        "EdVisitMonth",
        "Ethnicity",
        "HealthOutcomeID",
        "MonthlyVisits",
        "Race",
        "Sex"
      )
    )
  )


})
