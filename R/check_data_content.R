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
check_data_content <- function(data){

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

check_ethnicity_var <- function(data) {
  has_character <- NULL
  has_allowed_values <- NULL

  has_character <-
    checkmate::check_character(data$ethnicity) |>
    is.logical() |>
    purrr::set_names("class")

  if (has_character) {
    has_allowed_values <-
      checkmate::check_subset(
        data$ethnicity,
        c("H", "NH", "U")
      ) |>
      is.logical() |>
      purrr::set_names("allowed_values")
  }

  create_exit_status(
    "ethnicity",
    danger_variables = c(has_character, has_allowed_values)
  )
}

check_race_var <- function(data) {
  has_character <- NULL
  has_allowed_values <- NULL

  has_character <-
    checkmate::check_character(data$race) |>
    is.logical() |>
    purrr::set_names("class")

  if (has_character) {
    has_allowed_values <-
      checkmate::check_subset(
        data$race,
        c("W", "B", "O", "U")
      ) |>
      is.logical() |>
      purrr::set_names("allowed_values")
  }

  create_exit_status(
    "race",
    danger_variables = c(has_character, has_allowed_values)
  )
}

check_health_outcome_id_var <- function(data) {
  health_outcome_id <- NULL
  has_class <- FALSE
  has_allowed_values <- FALSE

  has_class <-
    is.character(data$health_outcome_id) || is.integer(data$health_outcome_id)

  #coerce to integer
  if (has_class) {
    data <-
      data |>
      dplyr::mutate(health_outcome_id = as.integer(health_outcome_id))

    has_allowed_values <-
      checkmate::check_subset(
        data$health_outcome_id,
        c(1:5),
      ) |>
      is.logical() |>
      purrr::set_names("allowed_values")
  }

  create_exit_status(
    "health_outcome_id",
    warn_variables = has_class,
    danger_variables = has_allowed_values
  )
}

check_monthly_count_var <- function(data) {
  monthly_count <- NULL
  has_class <- FALSE
  has_allowed_values <- FALSE

  has_class <-
    is.numeric(data$monthly_count) ||
    is.integer(data$monthly_count) ||
    is.character(data$monthly_count)

  if (has_class) {
    data <-
      data |>
      dplyr::mutate(monthly_count = as.integer(monthly_count))

    has_allowed_values <-
      checkmate::check_count(
        data$monthly_count,
        na.ok = FALSE,
        null.ok = FALSE
      ) |>
      is.logical() |>
      purrr::set_names("allowed_values")
  }

  create_exit_status(
    "monthly_count",
    warn_variables = has_class,
    danger_variables  = has_allowed_values
  )
}

check_sex_var <- function(data) {
  has_character <- NULL
  has_allowed_values <- NULL

  has_character <-
    checkmate::check_character(data$sex) |>
    is.logical() |>
    purrr::set_names("class")

  if (has_character) {
    has_allowed_values <-
      checkmate::check_subset(
        data$sex,
        c("M", "F", "U")
      ) |>
      is.logical() |>
      purrr::set_names("allowed_values")
  }

  create_exit_status(
    "sex",
    danger_variables = c(has_character, has_allowed_values)
  )
}

check_year_var <- function(data) {
  year <- NULL
  has_allowed_values <- FALSE
  has_class <- FALSE

  has_class <-
    is.numeric(data$year) ||
    is.integer(data$year) ||
    is.character(data$year)

  if (has_class) {
    data <-
      data |>
      dplyr::mutate(year = as.integer(year))

    has_allowed_values <-
      checkmate::check_integer(
        data$year,
        #EPHT founded in 2002
        lower = 2001,
        upper = 9999
      ) |>
      is.logical() |>
      purrr::set_names("allowed_values")
  }

  create_exit_status(
    "year",
    warn_variables = has_class,
    danger_variables = has_allowed_values
  )
}
