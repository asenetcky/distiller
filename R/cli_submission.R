
message_cli <- function(status_list) {

  if (status_list$code == 0) {
    cli::cli_alert_success(status_list$message)
  }

  if (status_list$code == 1) {
    cli::cli_alert_danger(status_list$message)
  }

  if (status_list$code == 2) {
    cli::cli_alert_warning(status_list$message)
  }

}


#TODO: for target variable name, use both the object name and the value in glue
create_exit_status <- function(
    target_variable_name,
    warn_variables = NULL,
    danger_variables = NULL) {

  exit_status <-
    dplyr::lst(
      code = 0,
      message = glue::glue("Success: {target_variable_name}")
    )

  if (!is.null(warn_variables)) {

    warn <- any(!warn_variables)

    if (warn) {
      exit_status <-
        dplyr::lst(
          code = 2,
          message = glue::glue("Warning: {target_variable_name} may not have correct format")
        )
    }
  }

  if (!is.null(danger_variables)) {

    danger <- any(!danger_variables)

    if (danger) {
      exit_status <-
        dplyr::lst(
          code = 1,
          message = glue::glue("Danger: {target_variable_name} is not allowable value")
        )
    }
  }
  exit_status
}
