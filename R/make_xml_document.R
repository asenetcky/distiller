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
#' Users are expected to start with a standard dataframe because the XML
#' requirements change based on the facility type (Hospital vs Emergency
#' Department) even though the variables and disease are the same. Users trade
#' some freedom in naming conventions and in return `distiller` abstracts away
#' the xml naming gymnastics. By having a single set of column names, users can
#' keep all their data in one place and do all the required wrangling on a
#' single set of data, all at once, and then use whatever package they wish to
#' split out the master data and iterate over it with `distiller`.
#'
#' Users have the option to check their submission at time of XML creation and
#' get instant feedback on the possible validity of their submission. Users can
#' further refine the returned xml document with their choice of package such as
#' [xml2](https://cran.r-project.org/web/packages/xml2/index.html),
#' [XML](https://cran.r-project.org/web/packages/XML/index.html) and friends.
#' When satisfied with the data product users will need to print the XML
#' document to a file and then submit to the CDC like they usually would.
#'
#' @section Data:
#' Users are expected to wrangle, aggregate and otherwise implement the logic
#' expected by EPHT themselves. `distiller` will not handle that step, though it
#' does provides some helpers: [collapse_race()], [collapse_ethnicity], and
#' [make_months_worse].
#'
#' In order for `distiller` to work properly there are some expectations about
#' the data that must be met:
#' * The data must be a dataframe or tibble

#' * For all `content_group_id` The data must have the following columns:
#'   (in any order):
#'   * month: character - acceptable values: "01", "02", "03" ... "12"
#'   * agegroup: numeric - acceptable values: 1-19
#'   * county: character - string length of 5, unless unknown, then county = "U"
#'   * ethnicity: character - acceptable values: "H", "NH", "U"
#'   * race: character - acceptable values: "W", "B", "O", "U"
#'   * health_outcome_id: numeric - acceptable values: 1-5
#'   * sex: character - acceptable values: "M", "F", "U"
#'   * year: numeric - acceptable values: 2001-9999
#'   * monthly_count: numeric - acceptable values: >0 and _no_ missing values
#' * For `content_group_id` "CO-ED" and "CO-HOSP" the data must have the
#' additional columns:
#'   * fire_count: numeric - acceptable values: >0 and _no_ missing values
#'   * nonfire_count: numeric - acceptable values: >0 and _no_ missing values
#'   * unknown_count: numeric - acceptable values: >0 and _no_ missing values
#'
#' @section Content Group Identifier:
#' The Content Group Identifier is the ID expected to be used by EPHT. It is
#' a combination of the the disease and facility type.  Details on which ID to
#' use can be found in the how-to-guides provided by EPHT.
#'
#' `content_group_id` must belong to one of the following:
#'  * "AS-ED", "AS-HOSP"
#'  * "MI-HOSP"
#'  * "CO-ED", "CO-HOSP"
#'  * "HEAT-ED", "HEAT-HOSP"
#'  * "COPD-ED", "COPD-HOSP"
#'
#' @section Metadata Control Number (mcn):
#' The Metadata control number is provided by the EPHT and is used to identify
#' the dataset and its content.  In order to submit data users will already
#' have a set of these.
#'
#' @section Submission Check:
#' If users set `check_first` = `TRUE` in [make_xml_document()] or run
#' [check_submission] or any of the other `check_* functions` then the a suite
#' of checks is run against the metadata, data structure and data content.
#' Please note that users do not need to run the whole suite of checks, they can
#' run each function piecemeal on their data as it is being prepared.
#'
#' [check_submission] is called which is a wrapper around
#' the following functions:
#' * [check_content_group_id]
#' * [check_data]
#' * [check_data_content]
#' * [check_jurisdiction_code]
#' * [check_mcn]
#' * [check_state_fips_code]
#' * [check_submitter_email]
#' * [check_submitter_name]
#' * [check_submitter_title]
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
#' Default is `FALSE`, if set to `TRUE` then `distiller` will run through its
#' suite of metadata and data checks.
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
#' doc
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

    # check metadata, data structure and data content
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

    # create empty xml document
    xml_document <- xml2::xml_new_document()

    # create proper root element based on facility type
    root_element <- make_root_element(content_group_id)

    # create header node based on provided metadata
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

    # convert data to row-based xml nodes
    dataset_node <- make_dataset_node(data, content_group_id)

    # add nodes to xml document
    xml2::xml_add_child(root_element, header_node)
    xml2::xml_add_child(root_element, dataset_node)
    xml2::xml_add_child(xml_document, root_element)


    xml_document
  }
