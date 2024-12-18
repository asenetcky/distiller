#' Make Header node for XML Document
#'
#' Create the Header Node of the XML document using the provided submission
#' metadata and tailor the XML format to match the supplied content group
#' identifier.
#'
#' @inheritParams make_xml_document
#'
#' @family xml
#' @return XML node
#' @export
#'
#' @examples
#' make_header_node(
#'   mcn = "12345678-1234-1234-1234-123456789012",
#'   jurisdiction_code = "CA",
#'   content_group_id = "XX-HOSP",
#'   submitter_email = "myname@email.com",
#'   submitter_name = "First Last",
#'   submitter_title = "Data Analyst",
#'   state_fips_code = "06"
#' )
make_header_node <- function(
    mcn,
    jurisdiction_code,
    content_group_id,
    submitter_email,
    submitter_name,
    submitter_title,
    state_fips_code) {
  # Create the header
  header_node <-
    xml2::read_xml("<Header></Header>")

  # add child sections
  xml2::xml_add_child(header_node, "MCN", mcn)
  xml2::xml_add_child(header_node, "JurisdictionCode", jurisdiction_code)
  xml2::xml_add_child(header_node, "ContentGroupIdentifier", content_group_id)

  # add section about person submitting the data
  submitter_node <-
    make_submitter_info(submitter_email, submitter_name, submitter_title)

  xml2::xml_add_child(header_node, submitter_node)
  xml2::xml_add_child(header_node, "StateFIPSCode", state_fips_code)

  header_node
}

make_submitter_info <- function(email, name, title) {
  submitter_node <- xml2::read_xml("<SubmitterInformation></SubmitterInformation>")
  xml2::xml_add_child(submitter_node, "SubmitterEmailAddress", email)
  xml2::xml_add_child(submitter_node, "SubmitterName", name)
  xml2::xml_add_child(submitter_node, "SubmitterTitle", title)
  submitter_node
}
