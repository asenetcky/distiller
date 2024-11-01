parse_content_group_id <- function(content_group_id) {
  #only allowable values so far
  type <-
    dplyr::case_when(
      content_group_id == "MI-HOSP" ~ "hosp",
      content_group_id == "CO-HOSP" ~ "hosp",
      content_group_id == "COPD-HOSP" ~ "hosp",
      content_group_id == "HEAT-HOSP" ~ "hosp",
      content_group_id == "AS-HOSP" ~ "hosp",
      content_group_id == "AS-ED" ~ "ed",
      content_group_id == "CO-ED" ~ "ed",
      content_group_id == "COPD-ED" ~ "ed",
      content_group_id == "HEAT-ED" ~ "ed",
      .default = "Unknown"
    )

  if (type == "Unknown") {
    stop("Unknown content_group_id")
  } else {
    type
  }
}

parse_health_outcome_id <- function(content_group_id) {
  #only allowable values so far
  health_outcome_id <-
    dplyr::case_when(
      content_group_id == "AS-ED" ~ 1,
      content_group_id == "AS-HOSP" ~ 1,
      content_group_id == "MI-HOSP" ~ 2,
      content_group_id == "CO-ED" ~ 3,
      content_group_id == "CO-HOSP" ~ 3,
      content_group_id == "HEAT-ED" ~ 4,
      content_group_id == "HEAT-HOSP" ~ 4,
      content_group_id == "COPD-ED" ~ 5,
      content_group_id == "COPD-HOSP" ~ 5,
      .default = NA
    )

  if (is.na(health_outcome_id)) {
    stop(paste("Unknown content_group_id: ", content_group_id))
  } else {
    health_outcome_id
  }
}

make_months_worse <- function(month_integer) {
  checkmate::assert_integerish(month_integer)

  if (month_integer < 1 | month_integer > 12) {
    rlang::warn(paste("Month integer is out of range: ", month_integer))
  }

  month_integer |>
    as.character() |>
    stringr::str_pad(width = 2, side = "left", pad = "0")
}


#TODO: for target variable name, use both the object name and the value in glue
create_exit_status <- function(
    target_variable_name,
    warn_variables = NULL,
    danger_variables = NULL) {

  exit_status <-
    dplyr::lst(
      code = 0,
      message = glue::glue("Success")
    )

  if (!is.null(warn_variables)) {

    warn <- any(!warn_variables)

    if (warn) {
    exit_status <-
      dplyr::lst(
        code = 2,
        message = glue::glue("Warning: {target_variable_name} may not have correct format")
      )
    }
  }

  if (!is.null(danger_variables)) {

    danger <- any(!danger_variables)

    if (danger) {
    exit_status <-
      dplyr::lst(
        code = 1,
        message = glue::glue("Danger: {target_variable_name} is not allowable value")
      )
    }
  }
  exit_status
}
