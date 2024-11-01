
# TODO something like this:
# see: https://cli.r-lib.org/reference/cli_progress_step.html
# f <- function() {
# msg <- ""
# cli_progress_step("Downloading data{msg}", spinner = TRUE)
# for (i in 1:100) {
#   Sys.sleep(2/100)
#   msg <- glue::glue(", got file {i}/100")
#   cli_progress_update()
# }
# cli_progress_step("Importing data")
# Sys.sleep(1)
# cli_progress_step("Cleaning data")
# Sys.sleep(2)
# cli_progress_step("Fitting model", spinner = TRUE)
# for (i in 1:100) { Sys.sleep(3/100); cli_progress_update() }
# }
# f()


#success
#warn
#danger
#info


#create exit status 0 for success
#exit status 1 for danger 2 for warn


## content_group_id
### has_character
### has_allowable_id

## mcn
### has_character
### has length
### has_format
#
# f <- function() {
#   size <- 0L
#   cli::cli_progress_step(
#     "Downloading data.",
#     msg_failed = "Downloaded {prettyunits::pretty_bytes(size)}.",
#     spinner = TRUE
#   )
#   for (i in 1:100) {
#     Sys.sleep(3/100)
#     size <- size + 8192
#     cli::cli_progress_update()
#   }
# }
# f()
#
#
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
#
# pkgs <- c("foo", "bar", "foobar")
# cli::cli_alert_success("Downloaded {length(pkgs)} packages.")


#TODO: for target variable name, use both the object name and the value in glue
create_exit_status <- function(
    target_variable_name,
    warn_variables = NULL,
    danger_variables = NULL) {

  exit_status <-
    dplyr::lst(
      code = 0,
      message = glue::glue("Success")
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
