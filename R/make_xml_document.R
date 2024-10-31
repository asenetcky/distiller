#' Create an XML document
#'
#' @param data Pre-wrangled dataframe
#' @param content_group_id Code that identifies the content
#' @param mcn Metadata Control Number provided by epht
#' @param jurisdiction_code Two-letter state abbreviation
#' @param state_fips_code FIPS code of the state
#' @param submitter_email Email of person submitting data to epht
#' @param submitter_name First and last name of person submitting data
#' @param submitter_title Title of person submitting data
#'
#' @return XML document
#' @export
#'
#' @examples
#' data <-
#' mtcars |>
#'   dplyr::rename(
#'     month = mpg,
#'     agegroup = cyl,
#'     county = disp,
#'     ethnicity = hp,
#'     health_outcome_id = drat,
#'     monthly_count = wt,
#'     race = qsec,
#'     sex = vs,
#'     year = am
#'   ) |>
#'   dplyr::select(-c(gear, carb))
#' content_group_id <- "AS-HOSP"
#' mcn <- "1234-1234-1234-1234-1234"
#' jurisdiction_code <- "two_letter_code"
#' state_fips_code <- "1234"
#' submitter_email <- "submitter@email.com"
#' submitter_name <- "Submitter Name"
#' submitter_title <- "Submitter Title"
#'doc <- make_xml_document(
#'  data,
#'  content_group_id,
#'  mcn,
#'  jurisdiction_code,
#'  state_fips_code,
#'  submitter_email,
#'  submitter_name,
#'  submitter_title
#')
make_xml_document <-
  function(data,
           content_group_id,
           mcn,
           jurisdiction_code,
           state_fips_code,
           submitter_email,
           submitter_name,
           submitter_title) {

  xml_document <- xml2::xml_new_document()

  root_element <- make_root_element(content_group_id)
  header_node <-
    make_header_node(
      mcn,
      jurisdiction_code,
      content_group_id,
      submitter_email,
      submitter_name,
      submitter_title,
      state_fips_code
  )

  dataset_node <- make_dataset_node(data, content_group_id)


  xml2::xml_add_child(root_element, header_node)
  xml2::xml_add_child(root_element, dataset_node)
  xml2::xml_add_child(xml_document, root_element)


  xml_document
}
