#TODO add function that collapses ethnicity
#TODO add function that collapses race
#TODO add function that formats and/or collapses sex



collapse_ethnicity <- function(ethnicity) {
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

  dplyr::case_when(
    std_eth == "hispanic" ~ "H",
    std_eth == "nonhispanic" ~ "NH",
    std_eth == "unknown" ~ "U",
    .default = "U"
  )
}

collapse_race <- function(race) {
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
    std_race == "unknown" ~ "U",
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
    .default = "U"
  )

}
