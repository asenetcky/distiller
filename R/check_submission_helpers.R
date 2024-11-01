
#TODO turn this into a nice little cli check list

check_data <- function(data, content_group_id) {

  expected_col_names <-
    c(
      "month",
      "agegroup",
      "county",
      "ethnicity",
      "health_outcome_id",
      "monthly_count",
      "race",
      "sex",
      "year"
    )

  additional_vars <-
    c(
      "fire_count",
      "nonfire_count",
      "unknown_count"
    )

  requires_additional_vars <-
    content_group_id %in% c("CO-ED", "CO-HOSP")

  checkmate::assert_data_frame(data)
  has_minimum_vars <- checkmate::check_subset(expected_col_names, names(data))
  has_additional_vars <- checkmate::check_subset(additional_vars, names(data))

  if (!is.logical(has_minimum_vars)) {
    cli::cli_alert_danger(
      "The data does not have the minimum required variables"
    )
    rlang::warn(has_minimum_vars)
  }

  if (requires_additional_vars & !is.logical(has_additional_vars)) {
    cli::cli_alert_warning(
      "The data does not have the additional required variables"
    )
    rlang::warn(has_additional_vars)
  }

  invisible(TRUE)
}


check_content_group_id <- function(content_group_id) {
  allowable_values <-
    c(
      "AS-ED",
      "AS-HOSP",
      "MI-HOSP",
      "CO-ED",
      "CO-HOSP",
      "HEAT-ED",
      "HEAT-HOSP",
      "COPD-ED",
      "COPD-HOSP"
    )

  has_character <-
    checkmate::check_character(
      content_group_id, null.ok = FALSE, any.missing = FALSE
    ) |>
    is.logical()

  has_allowable_id <- FALSE
  if (has_character) {
    has_allowable_id <- content_group_id %in% allowable_values
  }

  create_exit_status(
    "content_group_id",
    danger_variables = c(has_character, has_allowable_id)
  )
}


check_mcn <- function(mcn) {
  has_character <-
    checkmate::check_character(
      mcn, null.ok = FALSE, any.missing = FALSE
    ) |>
    is.logical()

  has_length <- FALSE
  has_format <- FALSE
  if (has_character) {

    # I don't know if this is always true
    has_length <-
      stringr::str_length(mcn) == 36

    # I don't know if this is always true
    has_format <-
      stringr::str_detect(
        string = mcn,
        pattern = "^[0-9\\w]{8}(-[0-9\\w]{4}){3}-[0-9\\w]{12}$"
      )
  }

  create_exit_status(
    "mcn",
    warn_variables = c(has_length, has_format),
    danger_variables = has_character
  )
}


check_jurisdiction_code <- function(jurisdiction_code) {
  has_character <-
    checkmate::check_character(
      jurisdiction_code,
      null.ok = FALSE,
      any.missing = FALSE
    ) |>
    is.logical()

  has_length <- FALSE
  has_format <- FALSE
  if (has_character) {
    has_length <-
      stringr::str_length(jurisdiction_code) == 2

    has_format <-
      stringr::str_detect(
        string = jurisdiction_code,
        pattern = "^[A-Z]{2}$"
      )
  }

  create_exit_status(
    "jurisdiction_code",
    warn_variables = c(has_length, has_format),
    danger_variables = has_character
  )
}

check_state_fips_code <- function(state_fips_code) {
  has_character <-
    checkmate::check_character(
      state_fips_code,
      null.ok = FALSE,
      any.missing = FALSE
    ) |>
    is.logical()

  has_length <- FALSE
  has_format <- FALSE
  if (has_character) {
    has_length <-
      stringr::str_length(state_fips_code) == 2

    has_format <-
      stringr::str_detect(
        string = state_fips_code,
        pattern = "^[0-9]{2}$"
      )
  }

  create_exit_status(
    "state_fips_code",
    warn_variables = c(has_length, has_format),
    danger_variables = has_character
  )
}

check_submitter_email <- function(submitter_email) {
  has_character <-
    checkmate::check_character(
      submitter_email,
      null.ok = FALSE,
      any.missing = FALSE
    ) |>
    is.logical()

  has_format <- FALSE
  if (has_character) {
#this is a simple check, not meant to be exhaustive
  has_format <-
    stringr::str_detect(
      string = submitter_email,
      pattern =
        "^\\S+@\\S+$"
      )
  }

  create_exit_status(
    "submitter_email",
    warn_variables = has_format,
    danger_variables = has_character
  )
}

check_submitter_name <- function(submitter_name) {
  has_character <-
    checkmate::check_character(
      submitter_name,
      null.ok = FALSE,
      any.missing = FALSE
    ) |>
    is.logical()

  has_format <- FALSE
  if (has_character) {
  #reasonably check if it is a first and last name
    has_format <-
      stringr::str_detect(
        string = submitter_name,
        pattern = "^[A-Z][a-z]+ [A-Z][a-z]+$"
      )
  }

  create_exit_status(
    "submitter_name",
    warn_variables = has_format,
    danger_variables = has_character
  )
}

check_submitter_title <- function(submitter_title) {
  has_character <-
    checkmate::check_character(
      submitter_title,
      null.ok = FALSE,
      any.missing = FALSE
    ) |>
    is.logical()

  has_length <- FALSE
  if (has_character) {
    has_length <-
      stringr::str_length(submitter_title) > 0
  }

  create_exit_status(
    "submitter_title",
    warn_variables = has_length,
    danger_variables = has_character
  )
}

# TODO check variables inside of data
