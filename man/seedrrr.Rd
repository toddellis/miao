% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seedrrr.R
\name{seedrrr}
\alias{seedrrr}
\title{seedrrr}
\usage{
seedrrr(x, seed = 8675309)
}
\arguments{
\item{x}{Piped workflow or dataset presumably being used with some RNG methods.}

\item{seed}{Random seed number to use. Defaults to everyone's favourite number.}
}
\value{
Function output with the random seed taken into account.
}
\description{
Set RNG seed within a workflow.
}
\examples{
rnorm(1) |> seedrrr()

}
