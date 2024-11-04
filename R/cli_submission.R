
message_cli <- function(status_list) {
  checkmate::assert_list(status_list)
  checkmate::assert_subset(names(status_list), c("code", "message"))

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
    is_named <-
      checkmate::check_names(names(warn_variables), type = "named") |>
      is.logical()

    if (warn) {
      if (is_named) {
        troublemakers <- list_troublemakers(warn_variables)
        exit_status <-
          dplyr::lst(
            code = 2,
            message = glue::glue(
              "Warning: {target_variable_name} may not have correct format
                Troublemakers: {troublemakers}"
            )
          )
      } else {
        exit_status <-
          dplyr::lst(
            code = 2,
            message = glue::glue(
              "Warning: {target_variable_name} may not have correct format"
            )
          )
      }
    }
    }


  if (!is.null(danger_variables)) {
    danger <- any(!danger_variables)
    is_named <-
      checkmate::check_names(names(danger_variables), type = "named") |>
      is.logical()

    if (danger) {
      if (is_named) {
        troublemakers <- list_troublemakers(danger_variables)
        exit_status <-
          dplyr::lst(
            code = 1,
            message = glue::glue(
              "Danger: {target_variable_name} does not have allowable value/s
                Troublemakers: {troublemakers}"
            )
          )

      } else {
       exit_status <-
        dplyr::lst(
          code = 1,
          message = glue::glue(
            "Danger: {target_variable_name} does not have allowable value/s"
          )
        )
      }
    }
  }
  exit_status
}

# just create the name vector specifically beforehand
list_troublemakers <- function(vars) {
  var_string <- ""
  vars <- which(!vars)
  for (var in names(vars)) {
    var_string <-
      glue::glue("{var_string}{var}, ")
  }

  var_string <- stringr::str_remove(var_string, ", $")
}
