% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_xml_document.R
\name{make_xml_document}
\alias{make_xml_document}
\title{Create an XML document with user-provided data and submission metadata}
\usage{
make_xml_document(
  data,
  content_group_id,
  mcn,
  jurisdiction_code,
  state_fips_code,
  submitter_email,
  submitter_name,
  submitter_title,
  check_first = FALSE
)
}
\arguments{
\item{data}{Pre-wrangled Dataframe.}

\item{content_group_id}{Code that identifies the content found in EPHT
documentation.}

\item{mcn}{Metadata Control Number provided by EPHT.}

\item{jurisdiction_code}{Two-letter state abbreviation for the submitter
state.}

\item{state_fips_code}{FIPS code of the submitter state.}

\item{submitter_email}{Email of person submitting data to EPHT.}

\item{submitter_name}{First and last name of person submitting data to EPHT.}

\item{submitter_title}{Title of person submitting data to EPHT.}

\item{check_first}{Check the validity of your EPHT submission.
Default is \code{FALSE}, if set to \code{TRUE} then \code{distiller} will run through its
suite of metadata and data checks.}
}
\value{
XML document object
}
\description{
This is the core function of \code{distiller}. It takes in a user-provided
dataframe with the expected columns and a set of metadata about the
submission. It then jumps through the XML hoops for the user and distills it
all down to an XML document that can be submitted to the CDC's EPHT
submission portal. Users should \emph{always} check the outputs with the EPHT test
submission portal first.

Users are expected to start with a standard dataframe because the XML
requirements change based on the facility type (Hospital vs Emergency
Department) even though the variables and disease are the same. Users trade
some freedom in naming conventions and in return \code{distiller} abstracts away
the xml naming gymnastics. By having a single set of column names, users can
keep all their data in one place and do all the required wrangling on a
single set of data, all at once, and then use whatever package they wish to
split out the master data and iterate over it with \code{distiller}.

Users have the option to check their submission at time of XML creation and
get instant feedback on the possible validity of their submission. Users can
further refine the returned xml document with their choice of package such as
\href{https://cran.r-project.org/web/packages/xml2/index.html}{xml2},
\href{https://cran.r-project.org/web/packages/XML/index.html}{XML} and friends.
When satisfied with the data product users will need to print the XML
document to a file and then submit to the CDC like they usually would.
}
\section{Data}{
Users are expected to wrangle, aggregate and otherwise implement the logic
expected by EPHT themselves. \code{distiller} will not handle that step, though it
does provides some helpers: \code{\link[=collapse_race]{collapse_race()}}, \code{\link[=collapse_ethnicity]{collapse_ethnicity()}}, and
\code{\link[=make_months_worse]{make_months_worse()}}.

In order for \code{distiller} to work properly there are some expectations about
the data that must be met:
\itemize{
\item The data must be a dataframe or tibble
\item For all \code{content_group_id} The data must have the following columns:
(in any order):
\itemize{
\item month: character - acceptable values: "01", "02", "03" ... "12"
\item agegroup: numeric - acceptable values: 1-19
\item county: character - string length of 5, unless unknown, then county = "U"
\item ethnicity: character - acceptable values: "H", "NH", "U"
\item race: character - acceptable values: "W", "B", "O", "U"
\item health_outcome_id: numeric - acceptable values: 1-5
\item sex: character - acceptable values: "M", "F", "U"
\item year: numeric - acceptable values: 2001-9999
\item monthly_count: numeric - acceptable values: >0 and \emph{no} missing values
}
\item For \code{content_group_id} "CO-ED" and "CO-HOSP" the data must have the
additional columns:
\itemize{
\item fire_count: numeric - acceptable values: >0 and \emph{no} missing values
\item nonfire_count: numeric - acceptable values: >0 and \emph{no} missing values
\item unknown_count: numeric - acceptable values: >0 and \emph{no} missing values
}
}
}

\section{Content Group Identifier}{
The Content Group Identifier is the ID expected to be used by EPHT. It is
a combination of the the disease and facility type.  Details on which ID to
use can be found in the how-to-guides provided by EPHT.

\code{content_group_id} must belong to one of the following:
\itemize{
\item "AS-ED", "AS-HOSP"
\item "MI-HOSP"
\item "CO-ED", "CO-HOSP"
\item "HEAT-ED", "HEAT-HOSP"
\item "COPD-ED", "COPD-HOSP"
}
}

\section{Metadata Control Number}{
The Metadata control number (mcn) is provided by the EPHT and is used to identify
the dataset and its content.  In order to submit data users will already
have a set of these.
}

\section{Submission Check}{
If users set \code{check_first} = \code{TRUE} in \code{\link[=make_xml_document]{make_xml_document()}} or runs
\code{\link[=check_submission]{check_submission()}} or any of the other \code{check_* functions} then the a suite
of checks is run against the metadata, data structure and data content.
Please note that users do not need to run the whole suite of checks, they can
run each function piecemeal on their data as it is being prepared.

\code{\link[=check_submission]{check_submission()}} is called which is a wrapper around
the following functions:
\itemize{
\item \code{\link[=check_content_group_id]{check_content_group_id()}}
\item \code{\link[=check_data]{check_data()}}
\item \code{\link[=check_data_content]{check_data_content()}}
\item \code{\link[=check_jurisdiction_code]{check_jurisdiction_code()}}
\item \code{\link[=check_mcn]{check_mcn()}}
\item \code{\link[=check_state_fips_code]{check_state_fips_code()}}
\item \code{\link[=check_submitter_email]{check_submitter_email()}}
\item \code{\link[=check_submitter_name]{check_submitter_name()}}
\item \code{\link[=check_submitter_title]{check_submitter_title()}}
}
}

\examples{
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

content_group_id <- "AS-HOSP"
mcn <- "1234-1234-1234-1234-1234"
jurisdiction_code <- "two_letter_code"
state_fips_code <- "1234"
submitter_email <- "submitter@email.com"
submitter_name <- "Submitter Name"
submitter_title <- "Submitter Title"

doc <- make_xml_document(
  data,
  content_group_id,
  mcn,
  jurisdiction_code,
  state_fips_code,
  submitter_email,
  submitter_name,
  submitter_title
)

doc
}
\seealso{
Other xml: 
\code{\link{make_dataset_node}()},
\code{\link{make_header_node}()},
\code{\link{make_root_element}()}
}
\concept{xml}
