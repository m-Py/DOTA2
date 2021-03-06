% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analytical_morbist.R
\name{domc_analytical}
\alias{domc_analytical}
\title{Probability that a DOMC item is solved correctly given accuracy and
criterion vector.}
\usage{
domc_analytical(accuracy, criterionVector, n_options = NULL, item = NULL,
  sd_sol = 1)
}
\arguments{
\item{accuracy}{Accuracy value of an assumed test taker}

\item{criterionVector}{The test taker's criterion vector; pass as in
\code{createTestTaker}}

\item{n_options}{number of answer options in the MC item; only pass
if item parameter is not specified}

\item{item}{Can be passed instead of n_options - pass a vector of 0
and 1, which indicate the sequence of answer options, in
particular the solution position}

\item{sd_sol}{Standard deviation of the solution
distribution. Distractor SD is always 1}
}
\value{
The expected probability that an item is solved correctly
}
\description{
This function can be used to analytically determine the expected
probability that a DOMC item is solved correctly, as opposed to the
simulation approach used in \code{workDOMCItem}
}
\author{
Martin Papenberg \email{martin.papenberg@hhu.de}
}
