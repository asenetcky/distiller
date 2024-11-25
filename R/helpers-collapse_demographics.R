#' Collapse Ethnicity based on EPHT logic
#'
#' Collapse common ethnicity values to the EPHT's allowable values:
#' "H", "NH", "U"
#'
#' If there are any other values, they will be converted to "U". `NULL` or
#' `NA` values can be converted to "U" or left as `NA_character_`
#'
#' # Ethnicity Values
#' Character strings are converted to lowercase and stripped of any non-word
#' characters. This is to ensure that the values are standardized before
#' comparison.
#'
#' Common values that get converted are:
#' * hispanic, hisp, h -> "H"
#' * nonhispanic, nonhisp, nh -> "NH"
#' * unknown, unk, u -> "U"
#'
#' @param ethnicity Character string of the ethnicity value.
#' @param na_is_unknown Convert NAs to "U" or leave as-is, default is `TRUE`.
#'
#' @family helpers
#' @return Ethnicity string or NA_character_
#' @export
#'
#' @examples
#' collapse_ethnicity("Hispanic")
collapse_ethnicity <- function(ethnicity, na_is_unknown = TRUE) {
  std_eth <- NULL
  std_eth <-
    stringr::str_trim(
      stringr::str_to_lower(
        stringr::str_remove_all(
          string = ethnicity,
          pattern = "\\W"
        )
      )
    )

  if (na_is_unknown) {
    dplyr::case_when(
      std_eth == "hispanic" ~ "H",
      std_eth == "hisp" ~ "H",
      std_eth == "h" ~ "H",
      std_eth == "nonhispanic" ~ "NH",
      std_eth == "nonhisp" ~ "NH",
      std_eth == "nh" ~ "NH",
      std_eth == "unknown" ~ "U",
      std_eth == "unk" ~ "U",
      .default = "U"
    )
  } else {
    dplyr::case_when(
      std_eth == "hispanic" ~ "H",
      std_eth == "hisp" ~ "H",
      std_eth == "h" ~ "H",
      std_eth == "nonhispanic" ~ "NH",
      std_eth == "nonhisp" ~ "NH",
      std_eth == "nh" ~ "NH",
      std_eth == "unknown" ~ "U",
      std_eth == "unk" ~ "U",
      std_eth == "u" ~ "U",
      .default = NA_character_
    )
  }
}

#' Collapse Race based on EPHT logic
#'
#' Collapse race down to the EPHT's allowable values: "W", "B", "O", "U"
#'
#' If there are any other values, they will be converted to "U". `NULL` or
#' `NA` values can be converted to "U" or left as `NA_character
#'
#' # Race Values
#' Character strings are converted to lowercase and stripped of any non-word
#' characters. This is to ensure that the values are standardized before
#' comparison.
#'
#' Common values that get converted are:
#' * white, caucasian -> "W"
#' * blackorafricanamerican, black, africanamerican -> "B"
#' * americanindianoralaskanative, americanindian, alaskanative -> "O"
#' * asian -> "O"
#' * nativehawaiianorotherpacificislander, nativehawaiian, otherpacificislander,
#'  pacificislander -> "O"
#' * other -> "O"
#' * unknown, unk, u -> "U"
#'
#' @param race Character string of the race value
#' @param na_is_unknown Convert NAs to "U" or leave as-is, default is `TRUE`.
#'
#' @family helpers
#' @return Race string or NA_character_
#' @export
#'
#' @examples
#' collapse_race("Asian")
collapse_race <- function(race, na_is_unknown = TRUE) {
  std_race <- NULL
  std_race <-
    stringr::str_trim(
      stringr::str_to_lower(
        stringr::str_remove_all(
          string = race,
          pattern = "\\W"
        )
      )
    )

  if (na_is_unknown) {
    dplyr::case_when(
      std_race %in% c(
        "white",
        "caucasian"
      ) ~ "W",
      std_race %in% c(
        "blackorafricanamerican",
        "black",
        "africanamerican"
      ) ~ "B",
      std_race %in% c(
        "unknown",
        "unk",
        "u"
      ) ~ "U",
      std_race %in% c(
        "americanindianoralaskanative",
        "americanindian",
        "alaskanative",
        "asian",
        "nativehawaiianorotherpacificislander",
        "nativehawaiian",
        "otherpacificislander",
        "pacificislander",
        "other"
      ) ~ "O",
      .default = "U"
    )
  } else {
    dplyr::case_when(
      std_race %in% c(
        "white",
        "caucasian"
      ) ~ "W",
      std_race %in% c(
        "blackorafricanamerican",
        "black",
        "africanamerican"
      ) ~ "B",
      std_race %in% c(
        "americanindianoralaskanative",
        "americanindian",
        "alaskanative",
        "asian",
        "nativehawaiianorotherpacificislander",
        "nativehawaiian",
        "otherpacificislander",
        "pacislander",
        "other"
      ) ~ "O",
      std_race %in% c(
        "unknown",
        "unk",
        "u"
      ) ~ "U",
      .default = NA_character_
    )
  }
}
