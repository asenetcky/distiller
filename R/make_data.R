#' Make the root element: HospitalizationData or EmergencyDepartmentData
#'
#' @param content_group_id Code that identifies the content
#'
#' @return XML node
#' @export
#'
#' @examples
#' make_data("MI-HOSP")
make_data <- function(content_group_id) {

type <- parse_content_group_id(content_group_id)

if (type == "hosp") {
  data_node <-
    xml2::read_xml(
      paste0(
        '<HospitalizationData xsi:schemaLocation="http://www.ephtn.org/NCDM/PH/HospitalizationData ',
        'ephtn-ph-HospitalizationData.xsd" xmlns="http://www.ephtn.org/NCDM/PH/HospitalizationData" ',
        'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
        '</HospitalizationData>'
      )
    )

} else if (type == "ed") {
  data_node <-
    xml2::read_xml(
      paste0(
        '<EmergencyDepartmentData ',
        'xmlns="http://www.ephtn.org/NCDM/PH/EmergencyDepartmentData">',
        '</EmergencyDepartmentData>'
      )
    )
} else {
  stop("Unknown content_group_id")
}

data_node

}

parse_content_group_id <- function(content_group_id) {
  #only allowable values
  type <-
    dplyr::case_when(
      content_group_id == "MI-HOSP" ~ "hosp",
      content_group_id == "CO-HOSP" ~ "hosp",
      content_group_id == "COPD-HOSP" ~ "hosp",
      content_group_id == "HEAT-HOSP" ~ "hosp",
      content_group_id == "AS-ED" ~ "ed",
      content_group_id == "AS-HOSP" ~ "ed",
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
