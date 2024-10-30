
parse_content_group_id <- function(content_group_id) {
  #only allowable values
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
