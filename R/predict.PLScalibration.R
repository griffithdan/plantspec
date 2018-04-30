#' Prediction method for \code{PLScalibration}.
#' 
#' This function accepts a PLS model of class \code{PLScalibration} and new
#' data as a \code{spectra.matrix}. It uses the pls::predict.mvr method for
#' predicting new data and also provides a mahalanobis distance (to the
#' multivariate center of the calibration dataset) for each new sample.
#' 
#' 
#' @param object An object of class \code{PLScalibration} containing the model.
#' @param newdata New data (scans) in the same units and resolution as the
#' spectra used to fit the PLS model. These spectra should be raw (without
#' preprocessing) if the spectra used to fit the model were not manually
#' manipulated before the use of \code{optimizePLS()} or \code{calibrate()}.
#' \code{predictPLS()} will automaticall conduct the appropriate preprocessing
#' and spectral subsetting if these steps were conducted within
#' \code{calibrate()}.
#' @param ... Additional args.
#' @return Returns named a vector of predicted values with the following
#' attributes:\cr "mahalanobis" - The mahalanobis distance of each new spectrum
#' from the calibration mean.\cr "mahalanobis_threshold" - The maximum
#' mahalanobis distance in the calibration dataset.\cr "outlier" - Logical.
#' TRUE is "mahalanobis" is greater than "mahalanobis_threshold."
#' @author Daniel M Griffith
#' @keywords calibration
#' @examples
#' 
#' 
#' #data(shootout)
#' #data(N_cal_shootout)
#' 
#' #shootout_scans <- convertSpectra(x = shootout_scans, method = "WL_to_WN")
#' 
#' #N_predicted <- predict(object = N_cal_shootout, newdata = shootout_scans)
#' #hist(N_predicted)
#' 
#' @export predict.PLScalibration
#' @export 
predict.PLScalibration <- function(object, newdata, ...){
    
  predictPLS(object = object, newdata = newdata, ...)
    
}
