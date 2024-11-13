test_that("check_data_content works", {

  # month = mpg,
  # agegroup = cyl,
  # county = disp,
  # ethnicity = hp,
  # health_outcome_id = drat,
  # monthly_count = wt,
  # race = qsec,
  # sex = vs,
  # year = am

  good_data_min_vars <-
    dplyr::tibble(
      month = rep("12", 12),
      agegroup = 1:12,
      county = as.character(12345:12356),
      ethnicity = rep("H",12),
      race = rep("O",12),
      health_outcome_id = rep(1,12),
      monthly_count = rep(1,12),
      year = rep(2024, 12),
      sex = rep("M", 12)
    )

  good_data_max_vars <-
    dplyr::tibble(
      fire_count = rep(1,12),
      nonfire_count = rep(1,12),
      unknown_count = rep(1,12)
    ) |>
    dplyr::bind_cols(good_data_min_vars)
  #purrr::map(purrr::pluck("code")) |> purrr::list_c()
  # good data is good
  expect_equal(
    check_data_content(good_data_min_vars, "AS-HOSP") |>
      purrr::map(purrr::pluck("code")) |>
      purrr::list_c() |>
      sum(),
    0
  )
})
