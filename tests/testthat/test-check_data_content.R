test_that("check_data_content works", {
  good_data_min_vars <-
    dplyr::tibble(
      month = rep("12", 12),
      agegroup = 1:12,
      county = as.character(12345:12356),
      ethnicity = rep("H", 12),
      race = rep("O", 12),
      health_outcome_id = rep(1, 12),
      monthly_count = rep(1, 12),
      year = rep(2024, 12),
      sex = rep("M", 12)
    )

  good_data_max_vars <-
    dplyr::tibble(
      fire_count = rep(1, 12),
      nonfire_count = rep(1, 12),
      unknown_count = rep(1, 12)
    ) |>
    dplyr::bind_cols(good_data_min_vars)

  bad_data_min_vars <-
    dplyr::tibble(
      month = rep("bad_data", 12),
      agegroup = rep("bad_data", 12),
      county = rep("bad_data", 12),
      ethnicity = rep("8", 12),
      race = rep("00", 12),
      health_outcome_id = rep(99, 12),
      monthly_count = rep(-1, 12),
      year = rep(1990, 12),
      sex = rep("Q", 12)
    )

  bad_data_max_vars <-
    dplyr::tibble(
      fire_count = rep("bad_data", 12),
      nonfire_count = rep("bad_data", 12),
      unknown_count = rep("bad_data", 12)
    ) |>
    dplyr::bind_cols(bad_data_min_vars)

  # good data is good
  expect_equal(
    check_data_content(good_data_min_vars, "AS-HOSP") |>
      purrr::map_int(purrr::pluck("code")) |>
      sum(),
    0
  )

  expect_equal(
    check_data_content(good_data_max_vars, "CO-HOSP") |>
      purrr::map_int(purrr::pluck("code")) |>
      sum(),
    0
  )

  # bad data is bad
  expect_true(
    check_data_content(bad_data_min_vars, "AS-HOSP") |>
      purrr::map_int(purrr::pluck("code")) |>
      sum() >= 9
  )

  expect_true(
    check_data_content(bad_data_max_vars, "CO-HOSP") |>
      purrr::map_int(purrr::pluck("code")) |>
      sum() >= 12
  )
})
