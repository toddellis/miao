% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bind_limits.R
\name{bind_limits}
\alias{bind_limits}
\title{bind_limits}
\usage{
bind_limits(x, lwr = 0, upr = 1, inverse = FALSE)
}
\arguments{
\item{x}{Numeric vector.}

\item{lwr}{Lower bounds for transforming x.}

\item{upr}{Upper bounds for transforming x.}

\item{inverse}{Logical for backtransforming the data to the original scale.}
}
\value{
A vector of the input transformed to recognize upper and lower bound limits.
}
\description{
Set lower and upper bounds for data. E.g., 0-100% to fit a logistic curve.
}
\examples{
tibble::tibble(num = seq(0.1, 0.8, by = 0.05)) \%>\%
  mutate(trans = bind_limits(num, lwr = 0, upr = 0.81)) \%>\%
  mutate(back = bind_limits(trans, lwr = 0, upr = 0.81, inverse = TRUE))

}
