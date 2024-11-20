
# grab the facility type from the content_group_id
parse_content_group_id <- function(content_group_id) {
  # only allowable values so far
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

#' Return the health outcome id based on the content group id
#'
#' The EPHT requires that each row node in the XML document have a health
#' outcome identifier.
#'
#' @inheritParams make_xml_document
#'
#' @family helpers
#' @return Integer health outcome identifier
#' @export
#'
#' @examples
#' parse_health_outcome_id("AS-HOSP")
parse_health_outcome_id <- function(content_group_id) {
  # only allowable values so far
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

#' Convert integer month to string month with leading zero
#'
#' @param month_integer Integer month
#'
#' @family helpers
#' @return String month with leading zero
#' @export
#'
#' @examples
#' make_months_worse(1)
make_months_worse <- function(month_integer) {
  checkmate::assert_integerish(month_integer)

  month_integer |>
    as.character() |>
    stringr::str_pad(width = 2, side = "left", pad = "0")
}
