
<!-- README.md is generated from README.Rmd. Please edit that file -->

# distiller

<!-- badges: start -->

[![R-CMD-check](https://github.com/asenetcky/distiller/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asenetcky/distiller/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/asenetcky/distiller/graph/badge.svg)](https://app.codecov.io/gh/asenetcky/distiller)
<!-- badges: end -->

Right off the bat I want to say I am in no way affiliated with the CDC.
As someone who now has to submit data to the CDC’s EPHT program I was
dismayed to find out that the documentation is highly convoluted and in
many cases, conflicts with itself. When reaching out to support, they
tell you to read the documentation, which is not helpful. Also, some of
the provided tooling *does not* work if you go through the instructions
provided and do everything in the order as presented. Basically folks
who have been doing this for a while are fine, or have made their peace
with this process and then all the newbies, like me are just endlessly
sending data to the test portal and waiting and waiting and waiting for
results.

My goal is to make this process easier and reproducible for myself, and
others.

So who is this highly specific package for?

- Do you submit data to the CDC’s EPHT program?
- Do you hate documentation that shows variables in the examples that
  don’t exist in the data dictionary or vice-verse?
- Do you hate it when the how-to-guides very clearly show the variables
  in UPPERCASE but then the portal kicks your data back because it
  expects it to be in lowercase?
- Are you baffled by the XML design decisions and lack of consistency in
  the formatting between the different types of facilities?
- Do you hate it when you follow the directions verbatim when using CDC
  tooling, but it constantly throws null-pointer exceptions at you
  because the very first step you were supposed to do was at the end of
  the instruction guide?  
- Do you hate it even more when you reach out to support they tell you
  to read the manual, instead of fixing the manual or even their own
  bug?

If you answered yes to any of these questions, then this package might
be for you

## Installation

You can install the development version of distiller from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("asenetcky/distiller")
```

## Example

For the six or so people this package applies to, here is a basic
example of how to use it:

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
