% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/complete_csphoc.R
\name{complete_csphoc}
\alias{complete_csphoc}
\title{Complete aggregated US AMLR CS-PHOC data}
\usage{
complete_csphoc(x)
}
\arguments{
\item{x}{data frame, phocid census data. See Details for more info}
}
\value{
A data frame, completed by [tidyr::nesting((header_id,
census_date_start), species))]. The data frame will have the same column
names and types as \code{x}

[tidyr::nesting((header_id,
census_date_start), species))]: R:tidyr::nesting((header_id,\%0A\%20\%20census_date_start),\%20species))
}
\description{
Complete aggregated CS-PHOC data collected by the US AMLR program
}
\details{
Because the Cape Shirreff Phocid Census (CS-PHOC) data collected by
the US AMLR program is not explicit, meaning it does not always include
zero records for when a beach was surveyed and no animals were seen, these
data usually must be completed before being shared or used in analyses.
This function uses \code{\link[tidyr:complete]{tidyr::complete()}} with US AMLR CS-PHOC data that has
already been aggregated by header ID and species, i.e. there is only one
record for every header record and species combo.

When completing, count columns are filled with zeroes to match PI
instruction on when certain counts were recorded. Specifically:
\itemize{
\item The following counts are filled with zeroes for all US AMLR CS-PHOC data:
ad_female_count, ad_male_count, ad_unk_count, juv_female_count,
juv_male_count, juv_unk_count, pup_live_count
\item unk_unk_count is filled with zeroes for all elephant seal counts,
and beginning in the 2014-15 season for all other species
\item unk_female_count and unk_male_count are filled with zeroes
beginning in the 2017-18 seasons. This logic is why census_date_start is a
required.

In addition, \code{x} must meet the following requirements:
\item Contains the following columns:
header_id, census_date_start, location, species, ad_female_count,
ad_male_count, ad_unk_count, juv_female_count, juv_male_count,
juv_unk_count, pup_live_count, research_program
\item None of the following columns contain \code{NA} values:
header_id, census_date_start, location, species, research_program
\item The location column only has one unique value
(i.e., the location column values are all the same)
\item All records are USAMLR records,
i.e. \code{all(x$research_program == "USAMLR")}
\item \code{x} is already grouped and summed by header_id and species
\item No header ID has multiple start dates,
and no 2 distinct header IDs have the same start date
\item All species column values are one of \code{\link{pinniped.phocid.sp}}

Note that the columns by which the data frame is completed are left as is,
meaning that users can make these columns factors if desired.

The CS-PHOC repo (link below) contains an example use case
}
}
\examples{
header.id <- c(1, 1, 2, 2, 3, 2)
census.dates <- as.Date(c("2000-01-01", "2000-01-08", "2000-01-15"))

count.df <- data.frame(
  header_id = header.id,
  species = tamatoamlr::pinniped.phocid.sp[c(1, 2, 1, 3, 4, 2)],
  location = "test",
  ad_female_count = c(5, 3, 7, 3, 6, 3),
  ad_male_count = c(NA, 4, 2, NA, 0, 3),
  ad_unk_count = 0,
  juv_female_count = 0,
  juv_male_count = 0,
  juv_unk_count = 0,
  pup_live_count = 0,
  unk_female_count = NA_integer_,
  unk_male_count = NA_integer_,
  unk_unk_count = NA_integer_,
  research_program = "USAMLR",
  census_date_start = census.dates[header.id]
)

complete_csphoc(count.df)

}
\seealso{
\url{https://github.com/us-amlr/cs-phoc}
}
