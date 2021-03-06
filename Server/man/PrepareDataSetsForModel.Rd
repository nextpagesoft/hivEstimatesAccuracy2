% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PrepareDataSetsForModel.R
\name{PrepareDataSetsForModel}
\alias{PrepareDataSetsForModel}
\title{PrepareDataSetsForModel}
\usage{
PrepareDataSetsForModel(dt, splitBy = NULL, strata = NULL, listIndex = 0)
}
\arguments{
\item{dt}{Input data set as data.table object. Required.}

\item{splitBy}{Name of column with values to be used for separation of data sets. Optional.
Default = \code{NULL}}

\item{strata}{Character vector of strata names. Optional. Default = \code{NULL}}

\item{listIndex}{Index in the output list to use if 'splitBy' columns does not exist in the
input data set and cannot be used for indexing output data set. If NULL then output file set
is not indexed and returned directly. Optional. Default = 0.}
}
\value{
List of HIV models
}
\description{
Prepares data sets for HIV Model
}
\examples{
\dontrun{
  PrepareDataSetsForModel(dt, splitBy = 'Imputation', strata = 'Transmission')
}

}
