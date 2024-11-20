#' Make the root element: HospitalizationData or EmergencyDepartmentData
#'
#' @inheritParams make_xml_document
#'
#' @family xml
#' @inherit make_header_node return
#'
#' @export
#'
#' @examples
#' make_root_element("MI-HOSP")
make_root_element <- function(content_group_id) {
  type <- parse_content_group_id(content_group_id)

  if (type == "hosp") {
    data_node <-
      xml2::read_xml(
        paste0(
          '<HospitalizationData xsi:schemaLocation="http://www.ephtn.org/NCDM/PH/HospitalizationData ',
          'ephtn-ph-HospitalizationData.xsd" xmlns="http://www.ephtn.org/NCDM/PH/HospitalizationData" ',
          'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
          "</HospitalizationData>"
        )
      )
  } else if (type == "ed") {
    data_node <-
      xml2::read_xml(
        paste0(
          "<EmergencyDepartmentData ",
          'xmlns="http://www.ephtn.org/NCDM/PH/EmergencyDepartmentData">',
          "</EmergencyDepartmentData>"
        )
      )
  } else {
    stop("Unknown content_group_id")
  }

  data_node
}
