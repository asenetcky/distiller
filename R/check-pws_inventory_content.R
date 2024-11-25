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

# given data check the class, format and length of data$pwsid_number
check_pwisid_number_var <- function(data) {
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

# given data, check the class and values ofdata$year_associated_to
check_year_associated_to_var <- function(data) {
  year_associated_to <- NULL
  check_year_var(data$year_associated_to)
}

# given data, check the class and values of data$year_pulled
check_year_pulled_var <- function(data) {
  year_pulled <- NULL
  check_year_var(data$year_pulled)
}

# given data, check the class and length of data$pws_name
check_pws_name_var <- function(data) {
  pws_name <- NULL

  has_length <-
    FALSE |>
    purrr::set_names("length")

  has_allowed_values <-
    FALSE |>
    purrr::set_names("allowed_values")

  has_character <-
    checkmate::check_character(
      data$pws_name,
      any.missing = FALSE,
      all.missing = FALSE,
      null.ok = FALSE
    ) |>
    is.logical() |>
    purrr::set_names("class")

  if (has_character) {
    has_length <-
      all(
        stringr::str_length(data$pws_name) <= 40
      ) |>
      purrr::set_names("length")

    # unknowns should be "U"
    has_proper_unknowns <-
      stringr::string_detect(
        stringr::str_to_lower(data$pws_name),
        "unknown",
        negate = TRUE
      ) |>
      all() |>
      purrr::set_names("has_proper_unknowns")

    # Not Submitted should be "NS"
    has_proper_not_submitted <-
      stringr::string_detect(
        stringr::str_to_lower(data$pws_name),
        "not submitted",
        negate = TRUE
      ) |>
      all() |>
      purrr::set_names("has_proper_not_submitted")

    has_allowed_values <-
      all(
        has_proper_unknowns,
        has_proper_not_submitted
      ) |>
      purrr::set_names("allowed_values")
  }

  create_exit_status(
    "pws_name",
    warn_variables = has_character,
    danger_variables = c(has_length, has_allowed_values)
  )
}

check_principal_county_served_fips_var <- function(data) {
  principal_county_served_fips <- NULL

  check_county_var(data$principal_county_served_fips)
}

check_principal_city_feature_id_var <- function(data) {
  principal_city_feature_id <- NULL

  # I don't know if there are any leading zeros
  # doesn't look like it from doco
  has_numeric <-
    checkmate::check_numeric(
      data$principal_city_feature_id,
      upper = 9999999999,
      all.missing = FALSE,
      any.missing = FALSE,
      null.ok = FALSE
    ) |>
    is.logical() |>
    purrr::set_names("class")

  # anything negative can only bee -888 or -999
  has_allowed_values <-
    all(
      data$principal_city_feature_id >= 0 |
        data$principal_city_feature_id == -888 |
        data$principal_city_feature_id == -999
    ) |>
    purrr::set_names("allowed_values")

  create_exit_status(
    "principal_city_feature_id",
    warn_variables = has_numeric,
    danger_variables = has_allowed_values
  )
}
