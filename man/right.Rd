% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/right.R
\name{right}
\alias{right}
\title{right}
\usage{
right(x, n = 4, pad = NULL)
}
\arguments{
\item{x}{Vector to extract from.}

\item{n}{Number of characters to extract starting from the right.}

\item{pad}{Character to pad string values with (from the right) if the input vectors are less than n.}
}
\value{
Transformed vector of `n` length.
}
\description{
Simplified variant of `substr` imitating the Excel & Alteryx `RIGHT` function with an option to pad.
}
\examples{
right(c('Example', 'E.g.', 'Eg'),
      n = 4,
      pad = ' ')

}
