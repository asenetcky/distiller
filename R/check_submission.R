
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
    )

  has_allowable_id <- content_group_id %in% allowable_values

  if (has_character & has_allowable_id) {
    exit_status <- 0
  } else {
    exit_status <- 1
  }
}




check_mcn <- function(mcn) {
  has_character <-
    checkmate::assert_character(
      mcn, null.ok = FALSE, any.missing = FALSE
    )

  # I don't know if this is always true
  has_length <-
    checkmate::checkTRUE(
      stringr::str_length(mcn) == 36,
    )

  # I don't know if this is always true
  has_format <-
    stringr::str_detect(
      string = mcn,
      pattern = "^[0-9\\w]{8}(-[0-9\\w]{4}){3}-[0-9\\w]{12}$"
    )

  if (has_character) {
    exit_status <- 0
  } else {
    exit_status <- 1
  }

  if (has_length & has_format) {
    exit_status <- 0
  } else {
    exit_status <- 1
  }




}




check_jurisdiction_code <- function(jurisdiction_code) {
  checkmate::assert_character(jurisdiction_code)

  check_length <-
    checkmate::checkTRUE(
      stringr::str_length(jurisdiction_code) == 2,
    )

  check_format <-
    stringr::str_detect(
      string = jurisdiction_code,
      pattern = "^[A-Z]{2}$"
    )

  if (!check_length) {
    cli::cli_alert_warning("The length of the jurisdiction code is not 2 characters")
  }

  if (!check_format) {
    cli::cli_alert_warning("The format of the jurisdiction code is not correct")
  }

  invisible(TRUE)
}

check_state_fips_code <- function(state_fips_code) {
  checkmate::assert_character(state_fips_code)

  check_length <-
    checkmate::checkTRUE(
      stringr::str_length(state_fips_code) == 2,
    )

  check_format <-
    stringr::str_detect(
      string = state_fips_code,
      pattern = "^[0-9]{2}$"
    )

  if (!check_length) {
    cli::cli_alert_warning("The length of the state fips code is not 2 characters")
  }

  if (!check_format) {
    cli::cli_alert_warning("The format of the state fips code is not correct")
  }

  invisible(TRUE)
}

check_submitter_email <- function(submitter_email) {
  checkmate::assert_character(submitter_email)

  check_format <-
    stringr::str_detect(
      string = submitter_email,
      pattern = "^SOMEPATTERN$"
    )

  if (!check_format) {
    cli::cli_alert_warning("The format of the submitter email is not correct")
  }

  invisible(TRUE)
}

check_submitter_name <- function(submitter_name) {
  checkmate::assert_character(submitter_name)

  #reasonably check if it is a first and last name
 check_format <-
    stringr::str_detect(
      string = submitter_name,
      pattern = "^[A-Z][a-z]+ [A-Z][a-z]+$"
    )

  if (!check_format) {
    cli::cli_alert_warning("The format of the submitter name is not correct")
  }

  invisible(TRUE)
}

check_submitter_title <- function(submitter_title) {
  checkmate::assert_character(submitter_title)

  #check that it isnt an empty string
  check_length <-
    checkmate::checkTRUE(
      stringr::str_length(submitter_title) > 0,
    )

  check_format <-
    stringr::str_detect(
      string = submitter_title,
      pattern = "^[A-Z][a-z]+$"
    )

  if (!check_format) {
    cli::cli_alert_warning("The format of the submitter title is not correct")
  }

  if (!check_length) {
    cli::cli_alert_warning("The submitter title is an empty string")
  }

  invisible(TRUE)
}

# TODO check variables inside of data


# TODO something like this:
# see: https://cli.r-lib.org/reference/cli_progress_step.html
# f <- function() {
# msg <- ""
# cli_progress_step("Downloading data{msg}", spinner = TRUE)
# for (i in 1:100) {
#   Sys.sleep(2/100)
#   msg <- glue::glue(", got file {i}/100")
#   cli_progress_update()
# }
# cli_progress_step("Importing data")
# Sys.sleep(1)
# cli_progress_step("Cleaning data")
# Sys.sleep(2)
# cli_progress_step("Fitting model", spinner = TRUE)
# for (i in 1:100) { Sys.sleep(3/100); cli_progress_update() }
# }
# f()
