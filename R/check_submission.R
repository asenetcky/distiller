check_submission <-
  function(data,
           content_group_id,
           mcn,
           jurisdiction_code,
           state_fips_code,
           submitter_email,
           submitter_name,
           submitter_title) {

    data_status <- check_data(data, content_group_id)
    content_group_id_status <- check_content_group_id(content_group_id)
    mcn_status <- check_mcn(mcn)
    jurisdiction_code_status <- check_jurisdiction_code(jurisdiction_code)
    state_fips_code_status <- check_state_fips_code(state_fips_code)
    submitter_email_status <- check_submitter_email(submitter_email)
    submitter_name_status <- check_submitter_name(submitter_name)
    submitter_title_status <- check_submitter_title(submitter_title)


    exit_status <-
      dplyr::lst(
        data_status,
        content_group_id_status,
        mcn_status,
        jurisdiction_code_status,
        state_fips_code_status,
        submitter_email_status,
        submitter_name_status,
        submitter_title_status
      )

    cli::cli_alert_info("Checking submission")
    purrr::walk(
      exit_status,
      message_cli
    )
  }
