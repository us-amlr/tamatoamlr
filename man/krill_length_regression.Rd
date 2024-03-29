% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/krill_length_regression.R
\name{krill_length_regression}
\alias{krill_length_regression}
\title{Krill length regressions}
\usage{
krill_length_regression(x, col.length, col.width)
}
\arguments{
\item{x}{data frame}

\item{col.length}{tidy-select (one unquoted expression) column with carapace
length values}

\item{col.width}{tidy-select (one unquoted expression) column with carapace
width values}
}
\value{
The data frame \code{x}, with the following additional columns. Note
that RCL = carapace length from scat, and RCW = carapace width from scat
\itemize{
\item D: an indicator of sex, calculated using D = -6.368 + 0.128RCL + 0.387RCW
\item sex: Sex of krill; 'F' (female) if D >= 0 and 'M' (male) if D < 0
\item krill_length: calculated using the following regressions for the different sexes:
female length = 11.6 + 2.13RCL, and male length = 0.62 + 3.13RCL
}
}
\description{
Calculate krill length from carapace measurements using sex-specific
regression models from Goebel et al. 2007.
}
\examples{
x <- data.frame(
  ID = 1:4,
  carapace_length = c(15, 12, 16, 17),
  carapace_width = c(8, 7, 9, 7)
)
krill_length_regression(x, carapace_length, carapace_width)

}
\references{
Goebel, M.E., Lipsky, J.D., Reiss, C.S. et al. Using carapace
measurements to determine the sex of Antarctic krill, Euphausia superba .
Polar Biol 30, 307–315 (2007). doi:10.1007/s00300-006-0184-8
}
\seealso{
\url{https://doi.org/10.1007/s00300-006-0184-8}
}
