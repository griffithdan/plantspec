#' Converts spectra into new units.
#' 
#' This function accepts spectra in a single \code{spectra.list} or
#' \code{spectra.matrix} object performs a specified units conversion.
#' 
#' 
#' @param x An object of class \code{spectra.list} or \code{spectra.matrix}.
#' @param method The desired conversion. Selected from:\cr "A_to_TR" -
#' Absorbance to Transmittance.\cr "TR_to_A" - Transmittance to Absorbance.\cr
#' "WN_to_WL" - Wavenumbers to Wavelength.\cr "WL_to_WN" - Wavelength to
#' Wavenumbers.
#' @return A \code{spectra.matrix} containing the converted spectra.
#' @author Daniel M Griffith
#' @keywords manipulation
#' @examples
#' 
#' 
#' #data(shootout)
#' #attributes(shootout_scans)
#' #converted_scans <- convertSpectra(x = shootout_scans, method = "WL_to_WN")
#' 
#' 
#' 
#' @export convertSpectra
convertSpectra <- function(x, method = NULL){
  
  if(class(x) == "spectra.list"){x <- as.spectra.matrix(x)}
  
  if(is.null(method)){stop("Please specify a method.")}else{

    if(method == "WN_to_WL"){colnames(x) <- (10^7/as.numeric(colnames(x))); attr(x, "wave_unit") <- "wavelength"}
    if(method == "WL_to_WN"){colnames(x) <- (10^7/as.numeric(colnames(x))); attr(x, "wave_unit") <- "wavenumber"}   
        
    if(method == "A_to_TR"){x <- 10 ^ (-x); attr(x, "measurement_unit") <- "transmittance"}
    if(method == "TR_to_A"){x <- -log(x); attr(x, "measurement_unit") <- "absorbance"}

    return(x)
  }
}
