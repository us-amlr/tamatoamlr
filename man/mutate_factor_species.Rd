% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutate_factor_species.R
\name{mutate_factor_species}
\alias{mutate_factor_species}
\title{Convert species column to a factor}
\usage{
mutate_factor_species(x, levels = names(tamatoamlr::pinniped.sp))
}
\arguments{
\item{x}{data frame that at minimum contains a 'species' column}

\item{levels}{character; passed to 'levels' argument of
\code{\link[base]{factor}}. Default is \code{names(\link{pinniped.sp})}}
}
\value{
\code{x}, with the column 'species' converted to a
\link[base]{factor} that has levels:
\code{\link[base]{names}(\link{pinniped.sp})}, i.e. \code{c("Fur seal",
  "Crabeater seal", etc.)}
}
\description{
A 'wrapper' around mutate that converts the species column to a factor with
the specified levels
}
\details{
A wrapper around \code{\link[dplyr]{mutate}} that converts the
species column to a factor with the specified levels. This allows this
function to be used directly in dplyr pipelines.
}
\examples{
x <- data.frame(species = c("Fur seal", "Weddell seal"), count = c(2, 1))
mutate_factor_species(x)
mutate_factor_species(x, levels = c("Fur seal", "Weddell seal", "Eseal"))

}
