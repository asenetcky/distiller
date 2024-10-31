









#TODO turn this into a nice little cli check list

check_data <- function(data, content_group_id) {

  expected_col_names <-
    c(
      "month",
      "agegroup",
      "county",
      "ethnicity",
      "health_outcome_id",
      "monthly_count",
      "race",
      "sex",
      "year"
    )

  additional_vars <-
    c(
      "fire_count",
      "nonfire_count",
      "unknown_count"
    )

  requires_additional_vars <-
    content_group_id %in% c("CO-ED", "CO-HOSP")

  checkmate::assert_data_frame(data)
  has_minimum_vars <- checkmate::check_subset(expected_col_names, names(data))
  has_additional_vars <- checkmate::check_subset(additional_vars, names(data))

  if (!is.logical(has_minimum_vars)) {
    cli::cli_alert_danger(
      "The data does not have the minimum required variables"
    )
    rlang::warn(has_minimum_vars)
  }

  if (requires_additional_vars & !is.logical(has_additional_vars)) {
    cli::cli_alert_warning(
      "The data does not have the additional required variables"
    )
    rlang::warn(has_additional_vars)
  }

  invisible(TRUE)
}



check_mcn <- function(mcn) {
  checkmate::assert_character(mcn)

# I don't know if this is always true
check_length <-
  checkmate::checkTRUE(
    stringr::str_length(mcn) == 36,
  )

# I don't know if this is always true
 check_format <-
  stringr::str_detect(
    string = mcn,
    pattern = "^[0-9\\w]{8}(-[0-9\\w]{4}){3}-[0-9\\w]{12}$"
  )

 #for now just warn about mcn characteristics
 if (!check_length) {
   cli::cli_alert_warning("The length of the mcn is not 36 characters")
 }

 if (!check_format) {
   cli::cli_alert_warning("The format of the mcn is not correct")
 }

 invisible(TRUE)
}


# TODO check content_group_id
# TODO check jurisidction_code
# TODO check state_fips_code
# TODO check submitter_email
# TODO check submitter_name
# TODO check submitter_title
# TODO check variables inside of data


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
