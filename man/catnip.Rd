% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catnip.R
\name{catnip}
\alias{catnip}
\title{catnip}
\usage{
catnip(data, x, conf_int = 0.95)
}
\arguments{
\item{x}{Numeric variable to summarise.}

\item{conf_int}{Confidence level for calculating mean confidence interval.}
}
\value{
Dataframe with useful summary statistics.
}
\description{
Calculates all the common summary statistics for reporting.
}
\examples{
runif(100, 0, 100) \%>\%
 round(0) \%>\%
 tibble::as_tibble() \%>\%
 dplyr::mutate(group = c(rep("GROUP A", 35), rep("GROUP B", 65))) \%>\%
 dplyr::group_by(group) \%>\%
 catnip(value)

}
