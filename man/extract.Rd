% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract}
\alias{extract}
\alias{tbl_vCensus_Phocid}
\alias{tbl_vCensus_AFS_Study_Beach}
\alias{tbl_vCensus_AFS_SAM}
\alias{tbl_vCensus_AFS_Capewide_Pup}
\alias{tbl_vCaptures}
\alias{tbl_vCaptures_Drugs}
\alias{tbl_vCaptures_Samples}
\alias{tbl_vCCAMLR_Pup_Weights}
\alias{tbl_vTag_Resights}
\alias{tbl_vTag_Resights_First_Per_Season}
\alias{tbl_vTag_Resights_Leopards}
\alias{tbl_vMicroVHF_Deployed}
\alias{tbl_vPinniped_Season}
\alias{tbl_vTakes}
\title{Extract data from database}
\usage{
tbl_vCensus_Phocid(src)

tbl_vCensus_AFS_Study_Beach(src)

tbl_vCensus_AFS_SAM(src)

tbl_vCensus_AFS_Capewide_Pup(src)

tbl_vCaptures(src)

tbl_vCaptures_Drugs(src)

tbl_vCaptures_Samples(src)

tbl_vCCAMLR_Pup_Weights(src)

tbl_vTag_Resights(src)

tbl_vTag_Resights_First_Per_Season(src)

tbl_vTag_Resights_Leopards(src)

tbl_vMicroVHF_Deployed(src)

tbl_vPinniped_Season(src)

tbl_vTakes(src)
}
\arguments{
\item{src}{a data source; likely a \code{\link[pool]{pool}} object
or some other connection to a database}
}
\description{
Functions used to extract pinniped data from database
}
\details{
These functions return the output of \code{\link[dplyr]{tbl}},
likely with at least some selecting or renaming.
Functions or modules using these functions will need to call
\code{link[dplyr]{collect}} to retrieve the data to R.

These functions provide a single place in the package where functions
that extract data from a database are located, making it easier to update
said functions if some aspect of the database structure changes.
}
