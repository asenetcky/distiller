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

  if (additional_vars) {
    type <- paste0(type, "_add_vars")

    expected_col_names <- c(
      expected_col_names,
      "fire_count",
      "nonfire_count",
      "unknown_count"
    )
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

  dataset_node
}
