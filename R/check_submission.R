#' Check the validity of a submission
#'
#' @param data dataframe to be converted to XML
#' @param content_group_id  Code that identifies the content
#' @param mcn Metadata Control Number provided by EPHT
#' @param jurisdiction_code Two-letter state abbreviation
#' @param state_fips_code FIPS code of the state
#' @param submitter_email Email of person submitting data to EPHT
#' @param submitter_name First and last name of person submitting data
#' @param submitter_title Title of person submitting data
#'
#' @return cli list in terminal of submission check results
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
#' check_submission(
#'   data,
#'   content_group_id,
#'   mcn,
#'   jurisdiction_code,
#'   state_fips_code,
#'   submitter_email,
#'   submitter_name,
#'   submitter_title
#' )
check_submission <-
  function(data,
           content_group_id,
           mcn,
           jurisdiction_code,
           state_fips_code,
           submitter_email,
           submitter_name,
           submitter_title) {
    content_group_id_status <- check_content_group_id(content_group_id)
    data_status <- check_data(data, content_group_id)
    data_content_status <- check_data_content(data, content_group_id)
    mcn_status <- check_mcn(mcn)
    jurisdiction_code_status <- check_jurisdiction_code(jurisdiction_code)
    state_fips_code_status <- check_state_fips_code(state_fips_code)
    submitter_email_status <- check_submitter_email(submitter_email)
    submitter_name_status <- check_submitter_name(submitter_name)
    submitter_title_status <- check_submitter_title(submitter_title)

    metadata_exit_status <-
      dplyr::lst(
        content_group_id_status,
        mcn_status,
        jurisdiction_code_status,
        state_fips_code_status,
        submitter_email_status,
        submitter_name_status,
        submitter_title_status
      )

    cli::cli_alert_info("Checking submission metadata")
    purrr::walk(
      metadata_exit_status,
      message_cli
    )

    cli::cli_alert_info("Checking data structure and content")
    data_exit_status <-
      c(
        dplyr::lst(data_status),
        data_content_status
      )

    purrr::walk(
      data_exit_status,
      message_cli
    )
  }
