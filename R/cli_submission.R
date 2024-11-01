
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
