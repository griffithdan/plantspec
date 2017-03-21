\name{N_cal_shootout}
\alias{N_cal_shootout}
\docType{data}
\title{
N_cal_shootout: Shootout NIR calibration for Nitrogen 
}
\description{
NIR calibration for Nitrogen using the \code{shootout} data.
}
\usage{data(N_cal_shootout)}
\format{
An object of class \code{PLScalibration}.

}

\references{

}
\examples{

#  library(leaf.spec)
#  library(leaf.spec.DB)

#######################################################################################################################
#######################################################################################################################
# N: Code to create this calibration
#######################################################################################################################
#######################################################################################################################
## LOAD TEST CALIBRATION DATA
#  data(shootout)
## INSPECT SPECTRA
#  plot(shootout_scans)
## CONVERT TO WAVENUMBERS
#  shootout_scans <- convertSpectra(x = shootout_scans, method = "WL_to_WN")
## SELECT TRAINING DATA FOR FITTING THE PLS MODEL
#  training_set <- !(subdivideDataset(spectra = shootout_scans, 
#                                     component = shootout_wetlab$N, 
#                                     method = "SPXY",
#                                     p=0.2, 
#                                     type = "validation"))
## OPTIMIZE SPECTRAL PREPROCESSING AND REGION SELECTION
#  N_opt <- optimizePLS(component = shootout_wetlab$N,
#                       spectra = shootout_scans,
#                       training_set = training_set)
## FIT A PLS MODEL, USING THE OPTIMAL PARAMETERS
#  N_cal_shootout <- calibrate(component=shootout_wetlab$N,
#                       spectra=shootout_scans,
#                       optimal_params=N_opt,
#                       optimal_model=1, 
#                       validation = "testset", 
#                       training_set = training_set)
## INSPECT CALIBRATION AND VALIDATION R2
#  N_cal_shootout$R2_Cal
#  N_cal_shootout$R2_Val

## PLOT FITTED vs TRUE VALUES 
#  plot(N_cal_shootout)

## LOAD CALIBRATION
# data(N_cal_shootout)


}
\keyword{calibrations}
