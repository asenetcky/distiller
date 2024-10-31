test_that("throws errors on improper arguments", {
  expect_error(make_dataset_node())
  expect_error(make_dataset_node(data = "huh", content_group_id = "AS-HOSP"))
  expect_error(make_dataset_node(data = "huh", content_group_id = "AS-ED"))
  expect_error(make_dataset_node(123))
  expect_error(make_dataset_node(lubridate::now()))

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

  expect_error(make_dataset_node(data = data_wrong_vars, content_group_id = "AS-HOSP"))
  expect_error(make_dataset_node(data = data_wrong_vars, content_group_id = "AS-ED"))
  expect_error(make_dataset_node(data = data_right_vars, content_group_id = "huh"))
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
    make_dataset_node(data = data_right_vars, content_group_id = "AS-HOSP"), "xml_node"
  )
  expect_s3_class(
    make_dataset_node(data = data_right_vars, content_group_id = "AS-ED"), "xml_node"
  )
  expect_s3_class(
    make_dataset_node(data = data_right_vars, content_group_id = "AS-HOSP"), "xml_document"
  )
  expect_s3_class(
    make_dataset_node(data = data_right_vars, content_group_id = "AS-ED"), "xml_document"
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
    make_dataset_node(data = data_right_vars, content_group_id = "AS-HOSP") |>
    xml2::as_list()

  xml_list_ed <-
    make_dataset_node(data = data_right_vars, content_group_id = "AS-ED") |>
    xml2::as_list()

  expect_equal(
    make_dataset_node(data = data_right_vars, content_group_id = "AS-HOSP") |>xml2::xml_name(),
    "Dataset"
  )
  expect_equal(
    make_dataset_node(data = data_right_vars, content_group_id = "AS-ED") |>  xml2::xml_name(),
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

test_that("Additional vars show up in the right order", {
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
      year = am,
      fire_count = gear,
      nonfire_count = carb
    ) |>
    dplyr::mutate(unknown_count = 1)

  xml_list_ed_add <-
    make_dataset_node(data_right_vars, content_group_id = "CO-ED") |>
    xml2::as_list()

  xml_list_hosp_add <-
    make_dataset_node(data_right_vars, content_group_id = "CO-HOSP") |>
    xml2::as_list()

  expect_equal(
    names(xml_list_ed_add$Dataset$Row |> dplyr::as_tibble()),
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
        "Sex",
        "IncidentCountFire",
        "IncidentCountNonFire",
        "IncidentCountUnknown"
      )
    )
  )

  expect_equal(
    names(xml_list_hosp_add$Dataset$Row |> dplyr::as_tibble()),
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
        "YearAdmitted",
        "IncidentCountFire",
        "IncidentCountNonFire",
        "IncidentCountUnknown"
      )
    )
  )


})

test_that("throws errors for missing vars", {
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
    #missing sex
    )
  expect_error(
    make_dataset_node(data, "AS-HOSP"),
    regexp = "Must be a permutation of set"
  )
  expect_error(
    make_dataset_node(data, "AS-ED"),
    regexp = "Must be a permutation of set"
  )
})
