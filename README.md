
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distiller

<!-- badges: start -->

[![R-CMD-check](https://github.com/asenetcky/distiller/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asenetcky/distiller/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/asenetcky/distiller/graph/badge.svg)](https://app.codecov.io/gh/asenetcky/distiller)
<!-- badges: end -->

## Motivation

As a newbie who has to submit data to the CDC’s EPHT program, I was
dismayed to find out that the documentation is buried under many layers
inside their SharePoint. It is also highly fragmented, convoluted and in
many cases, conflicts with itself.

My goal is to make this process easier and reproducible for myself, and
others.

So who is this highly specific package for?

- Do you submit data to the CDC’s EPHT program?
- Do you use R? Or are interested in incorporating R into your workflow?
- Do you struggle with the CDC’s EPHT documenation and/or tooling?
- Do you want to make your submission process more reproducible?

If you answered yes to the first question and any of the others, then
this package might be for you.

## Installation

You can install the development version of distiller from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("asenetcky/distiller")
```

## Example

Here is a basic example of how to use it:

``` r
library(distiller)

# Take you already-wrangled data
# note the specific variable names
data <-
    mtcars |>
    dplyr::rename(
      month = mpg,
      agegroup = cyl,
      county = disp,
      ethnicity = hp,
      health_outcome_id = drat,
      monthly_count = wt,
      race = qsec,
      sex = vs,
      year = am
    ) |>
    dplyr::select(-c(gear, carb))

# And your metadata
  content_group_id <- "AS-HOSP"
  mcn <- "1234-1234-1234-1234-1234"
  jurisdiction_code <- "two_letter_code"
  state_fips_code <- "1234"
  submitter_email <- "submitter@email.com"
  submitter_name <- "Submitter Name"
  submitter_title <- "Submitter Title"
  
# Optionally check your submission data structure and metadata
  check_submission(
    data,
    content_group_id,
    mcn,
    jurisdiction_code,
    state_fips_code,
    submitter_email,
    submitter_name,
    submitter_title
    )
#> ℹ Checking submission metadata
#> ✔ Success: content_group_id
#> ! Warning: mcn may not have correct format
#> Troublemakers: length, format
#> ! Warning: jurisdiction_code may not have correct format
#> Troublemakers: length, format
#> ! Warning: state_fips_code may not have correct format
#> Troublemakers: length, format
#> ✔ Success: submitter_email
#> ✔ Success: submitter_name
#> ✔ Success: submitter_title
#> ℹ Checking data structure and content
#> ✔ Success: dataframe_structure
#> ✖ Danger: month does not have allowable value/s
#> Troublemakers: allowed_values
#> ✔ Success: agegroup
#> ✖ Danger: county does not have allowable value/s
#> Troublemakers: length
#> ✖ Danger: ethnicity does not have allowable value/s
#> Troublemakers: allowed_values
#> ✖ Danger: health_outcome_id does not have allowable value/s
#> Troublemakers: allowed_values
#> ✖ Danger: sex does not have allowable value/s
#> Troublemakers: allowed_values
#> ✖ Danger: year does not have allowable value/s
#> Troublemakers: allowed_values
#> ✖ Danger: race does not have allowable value/s
#> Troublemakers: allowed_values
#> ✔ Success: monthly_count
# This can also be checked with `check_first = TRUE` in `make_xml_document()`
  

# And then make your xml document
make_xml_document(
    data,
    content_group_id,
    mcn,
    jurisdiction_code,
    state_fips_code,
    submitter_email,
    submitter_name,
    submitter_title
    )
#> {xml_document}
#> <HospitalizationData schemaLocation="http://www.ephtn.org/NCDM/PH/HospitalizationData ephtn-ph-HospitalizationData.xsd" xmlns="http://www.ephtn.org/NCDM/PH/HospitalizationData" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
#> [1] <Header>\n  <MCN>1234-1234-1234-1234-1234</MCN>\n  <JurisdictionCode>two_ ...
#> [2] <Dataset>\n  <Row>\n    <RowIdentifier>1</RowIdentifier>\n    <AdmissionM ...
```
