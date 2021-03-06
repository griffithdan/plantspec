% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrate.R
\name{calibrate}
\alias{calibrate}
\title{Fits a spectral PLS calibration model.}
\usage{
calibrate(component, spectra, optimal_params = NULL, optimal_model = 1,
  validation = "none", training_set = NULL, parallel = FALSE,
  max_comps = 10)
}
\arguments{
\item{component}{A vector of y-values. One for each spectrum.}

\item{spectra}{An object of class \code{spectra.matrix} containing spectra.
Rows should be in the same order as the component y-values.}

\item{optimal_params}{An object of class \code{PLSopt} containing the
optimization results from the \code{optimizePLS()} function.}

\item{optimal_model}{A row index (default = 1; i.e., the top model) for the
parameters desired in the \code{PLSopt} object.}

\item{validation}{What model validation should be taken, if any? ("none" =
no validation; "LOO" = leave-one-out cross validation; "testset" =
validation using a data subset specified as \code{FALSE} in
\code{training_set})}

\item{training_set}{A logical vector of \code{length(component)} specifying
\code{TRUE} for training/calibration data and \code{FALSE} for
test/validation set data}

\item{parallel}{Logical. The default is \code{FALSE}; \code{TRUE} allows for
the parallelization of validation proceedures, using the number of available
cores - 1. If \code{FALSE} the function will not be parallelized.
Parrallelization only applies for crossvalidation approaches.}

\item{max_comps}{What is the maximum number of latent vectors to possibly
include in the PLS regression. An integer.}
}
\value{
Returns an object of class \code{PLScalibration}. The object is a
list containing the pls model and the calibration/validation statistics. See
below:\cr

model - PLS model of class \code{mvr} form the \code{pls} package\cr rank -
the number of latent vectors (factors, ranks, etc) chosen for the model\cr
RMSEP - root mean squared error of prediction\cr R2_Cal - model calibration
R2\cr R2_Val - model validation R2\cr regions - the spectra regions used in
model fitting\cr preproc - the preprocessing steps used before model
fitting\cr training_set - logical indicating those spectra used in the
calibration fitting\cr data - data used in model fitting\cr
}
\description{
This function is a wrapper for \code{mvr()} in the pls package. The
function fits a Partial Least Squares (PLS) model relating a set of spectra
to a component variable. The function uses leave-one-out crossvalidation to 
calculate the optimal number of latent vectors to use in the PLS regression.
The optimal number of latent vectors is the minimum number of factors that 
result in a predicted residual error sum of squares (PRESS) statistic with 
a probability less than or equal to 0.75.
}
\examples{


# See main leaf.spec-package example. But:
\dontrun{
data(shootout)
temp <- calibrate(component = shootout_wetlab$N, spectra = shootout_scans, validation = "LOO")
}

}
\author{
Daniel M Griffith
}
\keyword{calibration}
