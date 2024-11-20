#' Check the validity of the data structure
#'
#' @param data dataframe
#' @param content_group_id Code that identifies the content
#'
#' @return list containing exit status code and success/failure message
#' @export
#'
#' @examples
#' check_data(mtcars, "AS-HOSP")
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

  has_dataframe <-
    checkmate::check_data_frame(data) |>
    is.logical() |>
    purrr::set_names("dataframe")


  if (requires_additional_vars) {
    expected_col_names <- c(expected_col_names, additional_vars)
  }

  has_vars <- FALSE
  if (has_dataframe) {
    has_vars <-
      checkmate::check_subset(expected_col_names, names(data)) |>
      is.logical() |>
      purrr::set_names("variables")
  }

  create_exit_status(
    "dataframe_structure",
    danger_variables = c(has_dataframe, has_vars)
  )
}


#' Check the  validity of a Content Group Identifier
#'
#' @param content_group_id Code that identifies the content
#'
#' @return list containing exit status code and success/failure message
#' @export
#'
#' @examples
#' check_content_group_id("AS-HOSP")
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
      content_group_id,
      null.ok = FALSE, any.missing = FALSE
    ) |>
    is.logical() |>
    purrr::set_names("class")

  has_allowable_id <- FALSE
  if (has_character) {
    has_allowable_id <-
      content_group_id %in% allowable_values |>
      purrr::set_names("id")
  }

  create_exit_status(
    "content_group_id",
    danger_variables = c(has_character, has_allowable_id)
  )
}


#' Check validity of a Metadata Control Number
#'
#' @param mcn Metadata Control Number provided by EPHT
#'
#' @return list containing exit status code and success/failure message
#' @export
#'
#' @examples
#' check_mcn("12345678-1234-1234-1234-123456789012")
check_mcn <- function(mcn) {
  has_character <-
    checkmate::check_character(
      mcn,
      null.ok = FALSE, any.missing = FALSE
    ) |>
    is.logical() |>
    purrr::set_names("class")

  has_length <- FALSE
  has_format <- FALSE
  if (has_character) {
    # I don't know if this is always true
    has_length <-
      stringr::str_length(mcn) == 36 |>
        purrr::set_names("length")

    # I don't know if this is always true
    has_format <-
      stringr::str_detect(
        string = mcn,
        pattern = "^[0-9\\w]{8}(-[0-9\\w]{4}){3}-[0-9\\w]{12}$"
      ) |>
      purrr::set_names("format")
  }

  create_exit_status(
    "mcn",
    warn_variables = c(has_length, has_format),
    danger_variables = has_character
  )
}


#' Check the validity of a jurisdiction code
#'
#' @param jurisdiction_code Two-letter state code
#'
#' @return list containing exit status code and success/failure message
#' @export
#'
#' @examples
#' check_jurisdiction_code("CA")
check_jurisdiction_code <- function(jurisdiction_code) {
  has_character <-
    checkmate::check_character(
      jurisdiction_code,
      null.ok = FALSE,
      any.missing = FALSE
    ) |>
    is.logical() |>
    purrr::set_names("class")

  has_length <- FALSE
  has_format <- FALSE
  if (has_character) {
    has_length <-
      stringr::str_length(jurisdiction_code) == 2 |>
        purrr::set_names("length")

    has_format <-
      stringr::str_detect(
        string = jurisdiction_code,
        pattern = "^[A-Z]{2}$"
      ) |>
      purrr::set_names("format")
  }

  create_exit_status(
    "jurisdiction_code",
    warn_variables = c(has_length, has_format),
    danger_variables = has_character
  )
}

#' Check the validity of a state FIPS code
#'
#' @param state_fips_code FIPS code of the state
#'
#' @return list containing exit status code and success/failure message
#' @export
#'
#' @examples
#' check_state_fips_code("06")
check_state_fips_code <- function(state_fips_code) {
  has_character <-
    checkmate::check_character(
      state_fips_code,
      null.ok = FALSE,
      any.missing = FALSE
    ) |>
    is.logical() |>
    purrr::set_names("class")

  has_length <- FALSE
  has_format <- FALSE
  if (has_character) {
    has_length <-
      stringr::str_length(state_fips_code) == 2 |>
        purrr::set_names("length")

    has_format <-
      stringr::str_detect(
        string = state_fips_code,
        pattern = "^[0-9]{2}$"
      ) |>
      purrr::set_names("format")
  }

  create_exit_status(
    "state_fips_code",
    warn_variables = c(has_length, has_format),
    danger_variables = has_character
  )
}

#' Check the validity of a submitter email
#'
#' @param submitter_email email address string
#'
#' @return list containing exit status code and success/failure message
#' @export
#'
#' @examples
#' check_submitter_email("myemail@email.com")
check_submitter_email <- function(submitter_email) {
  has_character <-
    checkmate::check_character(
      submitter_email,
      null.ok = FALSE,
      any.missing = FALSE
    ) |>
    is.logical() |>
    purrr::set_names("class")

  has_format <- FALSE
  if (has_character) {
    # this is a simple check, not meant to be exhaustive
    has_format <-
      stringr::str_detect(
        string = submitter_email,
        pattern =
          "^\\S+@\\S+$"
      ) |>
      purrr::set_names("format")
  }

  create_exit_status(
    "submitter_email",
    warn_variables = has_format,
    danger_variables = has_character
  )
}

#' Check the validity of a submitter name
#'
#' @param submitter_name First and last name of person submitting data
#'
#' @return list containing exit status code and success/failure message
#' @export
#'
#' @examples
#' check_submitter_name("Firstname Lastname")
check_submitter_name <- function(submitter_name) {
  has_character <-
    checkmate::check_character(
      submitter_name,
      null.ok = FALSE,
      any.missing = FALSE
    ) |>
    is.logical() |>
    purrr::set_names("class")


  has_format <- FALSE
  if (has_character) {
    # reasonably check if it is a first and last name
    has_format <-
      stringr::str_detect(
        string = submitter_name,
        pattern = "^[A-Z][a-z]+ [A-Z][a-z]+$"
      ) |>
      purrr::set_names("format")
  }

  create_exit_status(
    "submitter_name",
    warn_variables = has_format,
    danger_variables = has_character
  )
}

#' Check the validity of a submitter title
#'
#' @param submitter_title Job title of person submitting data
#'
#' @return list containing exit status code and success/failure message
#' @export
#'
#' @examples
#' check_submitter_title("Data Scientist")
check_submitter_title <- function(submitter_title) {
  has_character <-
    checkmate::check_character(
      submitter_title,
      null.ok = FALSE,
      any.missing = FALSE
    ) |>
    is.logical() |>
    purrr::set_names("class")

  has_length <- FALSE
  if (has_character) {
    has_length <-
      stringr::str_length(submitter_title) > 0 |>
        purrr::set_names("length")
  }

  create_exit_status(
    "submitter_title",
    warn_variables = has_length,
    danger_variables = has_character
  )
}
