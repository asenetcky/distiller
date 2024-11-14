#' Check the content of your data's variables
#'
#' @param data dataframe of wrangled data
#' @param content_group_id Code that identifies the content
#'
#' @return list of exit statuses for each variable
#' @export
#'
#' @examples
#' data <-
#'  mtcars |>
#'   dplyr::rename(
#'     month = mpg,
#'     agegroup = cyl,
#'     county = disp,
#'     ethnicity = hp,
#'     health_outcome_id = drat,
#'     monthly_count = wt,
#'     race = qsec,
#'     sex = vs,
#'     year = am
#'   ) |>
#'   dplyr::select(-c(gear, carb))
#'
#' check_data_content(data, "AS-HOSP")
check_data_content <- function(data, content_group_id){
  additional_vars <-
    dplyr::if_else(
      content_group_id %in% c("CO-ED", "CO-HOSP"),
      TRUE,
      FALSE,
      FALSE
    )

  non_count_functions <-
    dplyr::lst(
      check_month_var,
      check_agegroup_var,
      check_county_var,
      check_ethnicity_var,
      check_health_outcome_id_var,
      check_sex_var,
      check_year_var,
      check_race_var,
    )

  count_functions <- NULL
  if (additional_vars) {
    count_functions <-
      dplyr::lst(
        check_monthly_count_var,
        check_fire_count_var,
        check_nonfire_count_var,
        check_unknown_count_var
      )
  } else {
    count_functions <-
      dplyr::lst(check_monthly_count_var)
  }

  non_count_exit_status <-
    purrr::map(non_count_functions, \(fun) fun(data))

  count_exit_status <-
    purrr::map(count_functions, \(fun) fun(data))

  c(non_count_exit_status, count_exit_status)

}

check_month_var <- function(data) {
  month <- NULL

  allowable_values <-
    c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

  has_character <-
    checkmate::check_character(data$month) |>
    is.logical() |>
    purrr::set_names("class")

  has_allowed_values <- FALSE |>
    purrr::set_names("allowed_values")

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
    warn_variables = has_character,
    danger_variables = has_allowed_values
  )

}

check_agegroup_var <- function(data) {
  agegrouop <- NULL

  has_allowed_values <-
    FALSE |>
    purrr::set_names("allowed_values")

  has_class <-
    is.numeric(data$agegroup) |>
    purrr::set_names("class")

  if (has_class) {
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

  has_length <-
    FALSE |>
    purrr::set_names("length")

  has_character <-
    checkmate::check_character(data$county) |>
    is.logical() |>
    purrr::set_names("class")

  if (has_character) {
    has_unknown <-
      any("U" %in% data$county)

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
    warn_variables = has_character,
    danger_variables = has_length
  )
}

check_ethnicity_var <- function(data) {
  ethnicity <- NULL

  has_allowed_values <-
    FALSE |>
    purrr::set_names("allowed_values")

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
    warn_variables = has_character,
    danger_variables = has_allowed_values
  )
}

check_race_var <- function(data) {
  race <- NULL

  has_allowed_values <-
    FALSE |>
    purrr::set_names("allowed_values")

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
    warn_variables = has_character,
    danger_variables = has_allowed_values
  )
}

check_health_outcome_id_var <- function(data) {
  health_outcome_id <- NULL

  has_allowed_values <-
    FALSE |>
    purrr::set_names("allowed_values")

  has_class <-
    is.numeric(data$health_outcome_id) |>
    purrr::set_names("class")

  if (has_class) {
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


check_sex_var <- function(data) {
  sex <- NULL

  has_allowed_values <-
    FALSE |>
    purrr::set_names("allowed_values")

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
    warn_variables = has_character,
    danger_variables = has_allowed_values
  )
}

check_year_var <- function(data) {
  year <- NULL

  has_allowed_values <-
    FALSE |>
    purrr::set_names("allowed_values")

  has_class <-
    is.numeric(data$year) |>
    purrr::set_names("class")

  if (has_class) {
    has_allowed_values <-
      checkmate::check_numeric(
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


check_count_var <- function(data, var_name) {
  has_allowed_values <-
    FALSE |>
    purrr::set_names("allowed_values")

  count_var <-
    data |>
    dplyr::pull(var_name)

  has_class <-
    is.numeric(count_var) |>
    purrr::set_names("class")

  if (has_class) {
    has_allowed_values <-
      checkmate::check_numeric(
        count_var,
        lower = 0,
        any.missing = FALSE,
        all.missing = FALSE
      ) |>
      is.logical() |>
      purrr::set_names("allowed_values")
  }

  create_exit_status(
    var_name,
    warn_variables = has_class,
    danger_variables  = has_allowed_values
  )
}



check_monthly_count_var <- function(data) {
  check_count_var(data, "monthly_count")
}

check_fire_count_var <- function(data) {
  check_count_var(data, "fire_count")
}

check_nonfire_count_var <- function(data) {
  check_count_var(data, "nonfire_count")
}

check_unknown_count_var <- function(data) {
  check_count_var(data, "unknown_count")
}
