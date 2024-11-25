#' Make dataset xml node
#'
#' Create the Dataset Node of the XML document using the provided data and
#' content group identifier.  The data variables are converted to row-based
#' XML child nodes and the elements ordered and named according to the content
#' group identifer.
#'
#'
#' @inheritParams make_xml_document
#'
#' @family xml
#' @inherit make_header_node return
#' @export
#'
#' @examples
#' data_right_vars <-
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
#' make_dataset_node(data_right_vars, "AS-HOSP")
#'
make_dataset_node <- function(data, content_group_id) {
  type <- parse_content_group_id(content_group_id)

  # adding this extra infrastructure in case there are other
  # future edge cases with more/different vars
  # otherwise I'd just if() off of content_group_id & type
  additional_vars <-
    dplyr::if_else(
      content_group_id %in% c("CO-ED", "CO-HOSP"),
      TRUE,
      FALSE,
      FALSE
    )

  expected_col_names <-
    pick_var_set(content_group_id)

  if (additional_vars) {
    type <- paste0(type, "_add_vars")
  }

  checkmate::assert_data_frame(data)
  checkmate::assert_set_equal(names(data), expected_col_names, ordered = FALSE)
  checkmate::assert_choice(
    type,
    c("hosp", "ed", "hosp_add_vars", "ed_add_vars")
  )

  if (type == "hosp") {
    # Create the data node
    dataset_node <- xml2::read_xml("<Dataset></Dataset>")

    # Add the data
    for (i in seq_len(nrow(data))) {
      row_node <- xml2::read_xml("<Row></Row>")

      xml2::xml_add_child(row_node, "RowIdentifier", i)
      xml2::xml_add_child(row_node, "AdmissionMonth", data$month[i])
      xml2::xml_add_child(row_node, "AgeGroup", data$agegroup[i])
      xml2::xml_add_child(row_node, "County", data$county[i])
      xml2::xml_add_child(row_node, "Ethnicity", data$ethnicity[i])
      xml2::xml_add_child(row_node, "HealthOutcomeID", data$health_outcome_id[i])
      xml2::xml_add_child(row_node, "MonthlyHosp", data$monthly_count[i])
      xml2::xml_add_child(row_node, "Race", data$race[i])
      xml2::xml_add_child(row_node, "Sex", data$sex[i])
      xml2::xml_add_child(row_node, "YearAdmitted", data$year[i])

      xml2::xml_add_child(dataset_node, row_node)
    }
  }

  if (type == "hosp_add_vars") {
    # Create the data node
    dataset_node <- xml2::read_xml("<Dataset></Dataset>")

    # Add the data
    for (i in seq_len(nrow(data))) {
      row_node <- xml2::read_xml("<Row></Row>")

      xml2::xml_add_child(row_node, "RowIdentifier", i)
      xml2::xml_add_child(row_node, "AdmissionMonth", data$month[i])
      xml2::xml_add_child(row_node, "AgeGroup", data$agegroup[i])
      xml2::xml_add_child(row_node, "County", data$county[i])
      xml2::xml_add_child(row_node, "Ethnicity", data$ethnicity[i])
      xml2::xml_add_child(row_node, "HealthOutcomeID", data$health_outcome_id[i])
      xml2::xml_add_child(row_node, "MonthlyHosp", data$monthly_count[i])
      xml2::xml_add_child(row_node, "Race", data$race[i])
      xml2::xml_add_child(row_node, "Sex", data$sex[i])
      xml2::xml_add_child(row_node, "YearAdmitted", data$year[i])
      xml2::xml_add_child(row_node, "IncidentCountFire", data$fire_count[i])
      xml2::xml_add_child(row_node, "IncidentCountNonFire", data$nonfire_count[i])
      xml2::xml_add_child(row_node, "IncidentCountUnknown", data$unknown_count[i])

      xml2::xml_add_child(dataset_node, row_node)
    }
  }

  if (type == "ed_add_vars") {
    # Create the data node
    dataset_node <- xml2::read_xml("<Dataset></Dataset>")

    # Add the data
    for (i in seq_len(nrow(data))) {
      row_node <- xml2::read_xml("<Row></Row>")

      xml2::xml_add_child(row_node, "RowIdentifier", i)
      xml2::xml_add_child(row_node, "AgeGroup", data$agegroup[i])
      xml2::xml_add_child(row_node, "County", data$county[i])
      xml2::xml_add_child(row_node, "EdVisitYear", data$year[i])
      xml2::xml_add_child(row_node, "EdVisitMonth", data$month[i])
      xml2::xml_add_child(row_node, "Ethnicity", data$ethnicity[i])
      xml2::xml_add_child(row_node, "HealthOutcomeID", data$health_outcome_id[i])
      xml2::xml_add_child(row_node, "MonthlyVisits", data$monthly_count[i])
      xml2::xml_add_child(row_node, "Race", data$race[i])
      xml2::xml_add_child(row_node, "Sex", data$sex[i])
      xml2::xml_add_child(row_node, "IncidentCountFire", data$fire_count[i])
      xml2::xml_add_child(row_node, "IncidentCountNonFire", data$nonfire_count[i])
      xml2::xml_add_child(row_node, "IncidentCountUnknown", data$unknown_count[i])

      xml2::xml_add_child(dataset_node, row_node)
    }
  }

  if (type == "ed") {
    # Create the data node
    dataset_node <- xml2::read_xml("<Dataset></Dataset>")

    # Add the data
    for (i in seq_len(nrow(data))) {
      row_node <- xml2::read_xml("<Row></Row>")

      xml2::xml_add_child(row_node, "RowIdentifier", i)
      xml2::xml_add_child(row_node, "AgeGroup", data$agegroup[i])
      xml2::xml_add_child(row_node, "County", data$county[i])
      xml2::xml_add_child(row_node, "EdVisitYear", data$year[i])
      xml2::xml_add_child(row_node, "EdVisitMonth", data$month[i])
      xml2::xml_add_child(row_node, "Ethnicity", data$ethnicity[i])
      xml2::xml_add_child(row_node, "HealthOutcomeID", data$health_outcome_id[i])
      xml2::xml_add_child(row_node, "MonthlyVisits", data$monthly_count[i])
      xml2::xml_add_child(row_node, "Race", data$race[i])
      xml2::xml_add_child(row_node, "Sex", data$sex[i])

      xml2::xml_add_child(dataset_node, row_node)
    }
  }

  # TODO see if you cna use common names like year, month etc..
  if (type == "wql") {
    # Create the data node
    dataset_node <- xml2::read_xml("<Dataset></Dataset>")

    # Add the data
    for (i in seq_len(nrow(data))) {
      row_node <- xml2::read_xml("<Row></Row>")

      xml2::xml_add_child(row_node, "RowIdentifier", i)
      xml2::xml_add_child(row_node, "PWSIDNumber", data$pwsid_number[i])
      xml2::xml_add_child(row_node, "Year", data$year[i])
      xml2::xml_add_child(row_node, "AnalyteCode", data$analyte_code[i])
      xml2::xml_add_child(row_node, "DateSampled", data$date_sampled[i])
      xml2::xml_add_child(row_node, "AggregationType", data$agg_type[i])
      xml2::xml_add_child(row_node, "NumSamplingLocations", data$num_sampling_locations[i])
      xml2::xml_add_child(row_node, "SummaryTimePeriod", data$summary_time_period[i])
      xml2::xml_add_child(row_node, "NumSamples", data$num_samples[i])
      xml2::xml_add_child(row_node, "NumNonDetects", data$num_non_detects[i])
      xml2::xml_add_child(row_node, "ConcentrationUnits", data$concentration_units[i])
      xml2::xml_add_child(row_node, "Concentration", data$concentration[i])

      xml2::xml_add_child(dataset_node, row_node)
    }
  }

  if (type == "pws") {
    # Create the data node
    dataset_node <- xml2::read_xml("<Dataset></Dataset>")

    # Add the data
    for (i in seq_len(nrow(data))) {
      row_node <- xml2::read_xml("<Row></Row>")

      xml2::xml_add_child(row_node, "RowIdentifier", i)
      xml2::xml_add_child(row_node, "PWSIDNumber", data$pwsid_number[i])
      xml2::xml_add_child(row_node, "YearAssociatedTo", data$year_associated_to[i])
      xml2::xml_add_child(row_node, "YearPulled", data$year_pulled[i])
      xml2::xml_add_child(row_node, "PWSName", data$pws_name[i])
      xml2::xml_add_child(row_node, "PrincipalCountyServedFIPS", data$principal_county_served_fips[i])
      xml2::xml_add_child(row_node, "PrincipalCityFeatureID", data$principal_city_feature_id[i])
      xml2::xml_add_child(row_node, "TotalConnections", data$total_connections[i])
      xml2::xml_add_child(row_node, "SystemPopulation", data$system_population[i])
      xml2::xml_add_child(row_node, "PrimarySourceCode", data$primary_source_code[i])

      # this shows up in data, I don't know if it's required or not
      # I also don't know if this shows up in everyone's data or if they
      # have different inner nodes...

      # useless node within another node
      latitude_node <- xml2::read_xml("<Latitude></Latitude>")
      latitude_node <- xml2::xml_add_child(latitude_node, "LatitudeRange", data$latitude[i])

      # useless node within another node
      longitude_node <- xml2::read_xml("<Longitude></Longitude>")
      longitude_node <- xml2::xml_add_child(longitude_node, "LongitudeRange", data$longitude[i])

      xml2::xml_add_child(row_node, latitude_node)
      xml2::xml_add_child(row_node, longitude_node)

      xml2::xml_add_child(row_node, "LocationDerivationCode", data$location_derivation_code[i])

      xml2::xml_add_child(dataset_node, row_node)
    }
  }

  dataset_node
}

# given a content group id, return the expected variables
pick_var_set <- function(content_group_id) {
  # expected user-provided variable sets

  # hosp/ed
  ## for most hosp/ed
  standard_hosp_ed <-
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

  ## for the CO-related
  expanded_hosp_ed <-
    c(
      standard_hosp_ed,
      "fire_count",
      "nonfire_count",
      "unknown_count"
    )

  # drinkingwater-related
  # these are the "for-now" names until I find good common ones that can be
  # reused across the different datasets types
  ## water quality
  wql <-
    c(
      "pwsid_number",
      "year",
      "analyte_code",
      "date_sampled",
      "agg_type",
      "num_sampling_locations",
      "summary_time_period",
      "num_samples",
      "num_non_detects",
      "concentration_units",
      "concentration"
    )

  ## PWS? (find the definition of PWS)
  pws <-
    c(
      "pwsid_number",
      "year_associated_to",
      "year_pulled",
      "pws_name",
      "principal_county_served_fips",
      "principal_city_feature_id",
      "total_connections",
      "system_population",
      "primary_source_code",
      "latitude",
      "longitude",
      "location_derivation_code"
    )


  # return the right set of variables
  var_set <- NULL

  std_he_cgi <-
    c(
      "MI-HOSP",
      "COPD-HOSP",
      "HEAT-HOSP",
      "AS-HOSP",
      "AS-ED",
      "COPD-ED",
      "HEAT-ED"
    )

  if (content_group_id %in% c("CO-HOSP", "CO-ED")) {
    var_set <- expanded_hosp_ed
  }
  if (content_group_id %in% std_he_cgi) {
    var_set <- standard_hosp_ed
  }
  if (content_group_id == "WQL") {
    var_set <- wql
  }
  if (content_group_id == "PWS") {
    var_set <- pws
  }

  if (is.null(var_set)) {
    stop("Unknown content_group_id")
  } else {
    var_set
  }
}




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
