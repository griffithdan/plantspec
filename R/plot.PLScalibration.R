#' Visualize PLS modeling results.
#' 
#' This function is a wrapper to the plot methods in the pls R package. It
#' takes an object of class \code{PLScalibration} and produces diagnostic
#' plots.
#' 
#' 
#' @param x An object of class \code{PLScalibration} containing the model.
#' @param pch see par.
#' @param bg see par.
#' @param ncomp The number of latent vectors to use. If NULL, the default is to
#' use the optimal number of determined during the optimization process.
#' @param plottype One of:\cr "prediction" - Predicted versus observed.\cr
#' "validation" - RMSEP versus rank.\cr "coefficients" - Coefficients.\cr
#' "scores" - Biplot of scores.\cr "loadings" - Loadings.
#' @param ... Additional args.
#' @return A plot of model results.
#' @author Daniel M Griffith
#' @keywords visualization
#' @examples
#' 
#' 
#' #data(N_cal_shootout)
#' #plot(N_cal_shootout)
#' 
#' 
#' @export plot.PLScalibration
plot.PLScalibration <- function(x, pch = 21, bg = "gray", ncomp = NULL, plottype = "prediction", ...){
  
  plotPLScalibration(x = x, pch = pch, bg = bg, ncomp = ncomp, plottype = plottype, ...)
  
}
