#' Make dataset xml node
#'
#' @param .data Pre-wrangled dataframe
#' @param type Type of data: hospital or emergency department
#'
#' @return XML node
#' @export
#'
#' @examples
#'data_right_vars <-
#' mtcars |>
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
#' make_dataset(data_right_vars, "hosp")
#'
make_dataset <- function(.data, type) {

  col_names <-
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

  checkmate::assert_data_frame(.data)
  checkmate::assert_subset(names(.data), col_names)

  checkmate::assert_choice(type, c("hosp", "ed"))

  if (type == "hosp") {
    # Create the data node
    dataset_node <- xml2::read_xml("<Dataset></Dataset>")

    # Add the data
    for (i in seq_len(nrow(.data))) {
      row_node <- xml2::read_xml("<Row></Row>")

      xml2::xml_add_child(row_node, "RowIdentifier", i)
      xml2::xml_add_child(row_node, "AdmissionMonth", .data$month[i])
      xml2::xml_add_child(row_node, "AgeGroup", .data$agegroup[i])
      xml2::xml_add_child(row_node, "County", .data$county[i])
      xml2::xml_add_child(row_node, "Ethnicity", .data$ethnicity[i])
      xml2::xml_add_child(row_node, "HealthOutcomeID", .data$health_outcome_id[i])
      xml2::xml_add_child(row_node, "MonthlyHosp", .data$monthly_count[i])
      xml2::xml_add_child(row_node, "Race", .data$race[i])
      xml2::xml_add_child(row_node, "Sex", .data$sex[i])
      xml2::xml_add_child(row_node, "YearAdmitted", .data$year[i])

      xml2::xml_add_child(dataset_node, row_node)
    }
  }

  if( type == "ed") {
    # Create the data node
    dataset_node <- xml2::read_xml("<Dataset></Dataset>")

    # Add the data
    for (i in seq_len(nrow(.data))) {
      row_node <- xml2::read_xml("<Row></Row>")

      xml2::xml_add_child(row_node, "RowIdentifier", i)
      xml2::xml_add_child(row_node, "AgeGroup", .data$agegroup[i])
      xml2::xml_add_child(row_node, "County", .data$county[i])
      xml2::xml_add_child(row_node, "EdVisitYear", .data$year[i])
      xml2::xml_add_child(row_node, "EdVisitMonth", .data$month[i])
      xml2::xml_add_child(row_node, "Ethnicity", .data$ethnicity[i])
      xml2::xml_add_child(row_node, "HealthOutcomeID", .data$health_outcome_id[i])
      xml2::xml_add_child(row_node, "MonthlyVisits", .data$monthly_count[i])
      xml2::xml_add_child(row_node, "Race", .data$race[i])
      xml2::xml_add_child(row_node, "Sex", .data$sex[i])

      xml2::xml_add_child(dataset_node, row_node)
    }
  }
  dataset_node
}
