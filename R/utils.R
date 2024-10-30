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
      .default = "Unknown"
    )

  if (health_outcome_id == "Unknown") {
    stop("Unknown content_group_id")
  } else {
    health_outcome_id
  }
}

make_months_worse <- function(month_integer) {
  checkmate::assert_integer(month_integer)

  month_integer |>
    as.character() |>
    stringr::str_pad(width = 2, side = "left", pad = "0")
}
