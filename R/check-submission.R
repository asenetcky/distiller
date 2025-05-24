#' Check the validity of a submission
#'
#' @description
#' Check the metadata, data structure and data content of a potential submission
#' to the EPHT. `check_submission` is meant to be a tool to provide quick
#' feedback before users spend a ton of time waiting on the submission portal
#' results. Most checks are simple and non-comprehensive and meant to tip the
#' user off to potential problems.  Where there are explicit allowable
#' values provided by the cdc those has been included and the data content
#' will be checked against them.
#'
#' Users can expect to find the results of `check_submission` in the console
#' printed as a cli list. The results of each check will be one of 3 options
#' Success, Warning, or Danger.  None of the checks are going to prevent users
#' from moving forward with their submission.  They are merely reasonable
#' guidelines and should be treated as such.
#'
#' # Success
#' This check passes. However, requirements can change over time and this
#' package cannot be expected to catch everything up to the last minute.  This
#' is a good sign, but users should expect the test submission portal to have
#' the final say.
#'
#' # Warning
#' This check found something out of the ordinary.  Either the value wasn't in
#' the expected format, the severity of the issue doesn't rise to the level of
#' danger, or outside of increasing code complexity dramatically, this simple
#' check just wasn't sure if the provided value conformed requirements. The
#' test submission portal will have the final say.
#'
#' # Danger
#' Provided the package is up to date with EPHT requirements, this check
#' has found something this is wrong, like a value outside of anything in
#' the EPHT data dictionaries or the content of the value is not in a format
#' that is not going to play nicely with `distiller` or the data submission
#' portal. The test submission portal will have the final say.
#'
#' @inherit make_xml_document sections
#' @inheritParams make_xml_document
#' @family checks
#'
#' @return cli list in console of submission check results
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
#'
#' content_group_id <- "AS-HOSP"
#' mcn <- "1234-1234-1234-1234-1234"
#' jurisdiction_code <- "two_letter_code"
#' state_fips_code <- "1234"
#' submitter_email <- "submitter@email.com"
#' submitter_name <- "Submitter Name"
#' submitter_title <- "Submitter Title"
#'
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
