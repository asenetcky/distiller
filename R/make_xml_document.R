#' Create an XML document with user-provided data and submission metadata
#'
#' @description
#' This is the core function of `distiller`. It takes in a user-provided
#' dataframe with the expected columns and a set of metadata about the
#' submission. It then jumps through the XML hoops for the user and distills it
#' all down to an XML document that can be submitted to the CDC's EPHT
#' submission portal. Users should _always_ check the outputs with the EPHT test
#' submission portal first.
#'
#' Users have the option to check their submission at time of XML creation and
#' get instant feedback on the possible validity of their submission. Users can
#' further refine the returned xml document with their choice of package such as
#' [xml2](https://cran.r-project.org/web/packages/xml2/index.html),
#' [XML](https://cran.r-project.org/web/packages/XML/index.html) and friends.
#' When satisfied with the data product users will need to print the XML
#' document to a file and then submit to the CDC like they usually would.
#'
#'
#' @param data Pre-wrangled Dataframe.
#' @param content_group_id Code that identifies the content found in EPHT
#'   documentation
#' @param mcn Metadata Control Number provided by EPHT.
#' @param jurisdiction_code Two-letter state abbreviation for the submitter
#'   state.
#' @param state_fips_code FIPS code of the submitter state.
#' @param submitter_email Email of person submitting data to EPHT.
#' @param submitter_name First and last name of person submitting data to EPHT.
#' @param submitter_title Title of person submitting data to EPHT.
#' @param check_first Check the validity of your EPHT submission.
#'
#' @return XML document object
#' @export
#'
#' @examples
#' data <-
#'   mtcars |>
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
#' doc <- make_xml_document(
#'   data,
#'   content_group_id,
#'   mcn,
#'   jurisdiction_code,
#'   state_fips_code,
#'   submitter_email,
#'   submitter_name,
#'   submitter_title
#' )
make_xml_document <-
  function(data,
           content_group_id,
           mcn,
           jurisdiction_code,
           state_fips_code,
           submitter_email,
           submitter_name,
           submitter_title,
           check_first = FALSE) {
    if (check_first) {
      check_submission(
        data,
        content_group_id,
        mcn,
        jurisdiction_code,
        state_fips_code,
        submitter_email,
        submitter_name,
        submitter_title
      )
    }

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
