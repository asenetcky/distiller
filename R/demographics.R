
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
      std_eth == "nonhispanic" ~ "NH",
      std_eth == "unknown" ~ "U",
      .default = "U"
    )
  } else {
    dplyr::case_when(
      std_eth == "hispanic" ~ "H",
      std_eth == "nonhispanic" ~ "NH",
      std_eth == "unknown" ~ "U",
      .default = NA_character_
    )
  }

}

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
    std_race == "unknown" ~ "U",
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
    .default = NA_character_
  )
  }
}
