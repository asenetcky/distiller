# expected_col_names <-
#   c(
#     "month",
#     "agegroup",
#     "county",
#     "ethnicity",
#     "health_outcome_id",
#     "monthly_count",
#     "race",
#     "sex",
#     "year",
#     "fire_count",
#     "nonfire_count",
#     "unknown_count"
#   )

# going to be the big wrapper for all these
check_data_content <- function(){

}

check_month_var <- function(data) {
  allowable_values <-
    c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

  has_character <-
    checkmate::check_character(data$month) |>
    is.logical() |>
    purrr::set_names("class")

  has_allowed_values <- FALSE

  if (has_character) {
    has_allowed_values <-
      checkmate::check_subset(
        data$month,
        allowable_values
      ) |>
      is.logical() |>
      purrr::set_names("allowed_values")
  }

  create_exit_status(
    "month",
    danger_variables = c(has_allowed_values, has_character)
  )

}

check_agegroup_var <- function(data) {
  agegroup <- NULL
  has_class <- FALSE
  has_allowed_values <- FALSE

  has_class <-
    is.character(data$agegroup) || is.integer(data$agegroup)

  #coerce to integer
  if (has_class) {
    data <-
      data |>
      dplyr::mutate(agegroup = as.integer(agegroup))

  has_allowed_values <-
    checkmate::check_subset(
      data$agegroup,
      c(1:19),
    ) |>
    is.logical() |>
    purrr::set_names("allowed_values")
  }

  create_exit_status(
    "agegroup",
    warn_variables = has_class,
    danger_variables = has_allowed_values
  )
}

check_county_var <- function(data) {
  county <- NULL
  has_length <- FALSE
  has_character <- FALSE
  has_unknown <- FALSE

  has_character <-
    checkmate::check_character(data$county) |>
    is.logical() |>
    purrr::set_names("class")

  if (has_character) {
    has_unknown <-
      checkmate::check_subset(data$county, "U") |>
      is.logical() |>
      purrr::set_names("unknown")

    if (has_unknown) {
      data <-
        data |>
        dplyr::filter(county != "U")
    }

    has_length <-
      all(
        stringr::str_length(data$county) == 5
      ) |>
      purrr::set_names("length")
  }

  create_exit_status(
    "county",
    danger_variables = c(has_length, has_character)
  )
}
