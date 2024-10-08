% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samplingFunctions_r.R
\name{get_r_samples_from_population_data}
\alias{get_r_samples_from_population_data}
\title{Calculate a number of sample correlations based on a specified population correlation}
\usage{
get_r_samples_from_population_data(
  data = NA,
  n,
  number.of.samples = 10,
  number.of.decimals = 2,
  seed.value = 1
)
}
\arguments{
\item{data}{Population data - first two columns used for population correlation rho.}

\item{n}{Sample size for all samples (range of sample size if two specified)}

\item{number.of.samples}{Number of samples to obtain}

\item{number.of.decimals}{Number of decimals to report in returned data frame}

\item{seed.value}{random number seed value}
}
\value{
Data frame with sample correlations
}
\description{
Calculate a number of sample correlations based on a specified population correlation
}
