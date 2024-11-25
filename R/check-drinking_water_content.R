# xml names for
# pws
# "PWSIDNumber",
# "Year",
# "AnalyteCode",
# "DateSampled",
# "AggregationType",
# "NumSamplingLocations",
# "SummaryTimePeriod",
# "NumSamples",
# "NumNonDetects",
# "ConcentrationUnits",
# "Concentration"

# wql
# "PWSIDNumber",
# "YearAssociatedTo",
# "YearPulled",
# "PWSName",
# "PrincipalCountyServedFIPS",
# "PrincipalCityFeatureID",
# "TotalConnections",
# "SystemPopulation",
# "PrimarySourceCode",
# "Latitude",
# "Longitude",
# "LocationDerivationCode"

placeholder_example <- function(data) {
  county <- NULL

  has_length <-
    FALSE |>
    purrr::set_names("length")

  has_character <-
    checkmate::check_character(data$county) |>
    is.logical() |>
    purrr::set_names("class")

  if (has_character) {
    has_unknown <-
      any("U" %in% data$county)

    if (has_unknown) {
      data <-
        data |>
        dplyr::filter(county != "U")
    }

    has_length <-
      all(
        stringr::str_length(data$county) == 5
      ) |>
      purrr::set_names("length")
  }

  create_exit_status(
    "county",
    warn_variables = has_character,
    danger_variables = has_length
  )
}

check_pwisid_number <- function(data) {
  pwsid_number <- NULL

  has_length <-
    FALSE |>
    purrr::set_names("length")

  has_format <-
    FALSE |>
    purrr::set_names("format")

  has_character <-
    checkmate::check_character(data$pwsid_number) |>
    is.logical() |>
    purrr::set_names("class")

  if (has_character) {
    has_length <-
      all(
        stringr::str_length(data$pwsidnumber) == 9
      ) |>
      purrr::set_names("length")

    has_format <-
      all(
        stringr::str_detect(data$pwsid_number, "^[A-Z]{2}[0-9]{7}$")
      ) |>
      purrr::set_names("format")
  }

  create_exit_status(
    "pwsid_number",
    warn_variables = has_character,
    danger_variables = c(has_length, has_format)
  )
}
