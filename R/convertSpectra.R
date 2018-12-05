#' Converts spectra into new units.
#' 
#' This function accepts spectra in a single \code{spectra.list} or
#' \code{spectra.matrix} object performs a specified units conversion. 
#' Conversion between wavelengths and wave numbers are reliable.  However, 
#' conversions among transmittance, absorbance, and reflectance are
#' naive estimations (inverse log10 transformations) provided for convenience. 
#' 
#' @param x An object of class \code{spectra.list} or \code{spectra.matrix}.
#' @param method The desired conversion. Selected from:
#'   \cr "A_to_TR" - Absorbance to Transmittance.
#'   \cr "TR_to_A" - Transmittance to Absorbance.
#'   \cr "A_to_TR" - Absorbance to Transmittance.
#'   \cr "TR_to_A" - Transmittance to Absorbance.
#'   \cr "WN_to_WL" - Wavenumbers to Wavelength.
#'   \cr "WL_to_WN" - Wavelength to Wavenumbers.
#' @return A \code{spectra.matrix} containing the converted spectra.
#' @author Daniel M Griffith
#' @keywords manipulation
#' @examples
#' 
#' \dontrun{
#' data(shootout)
#' attributes(shootout_scans)
#' converted_scans <- convertSpectra(x = shootout_scans, method = "WL_to_WN")
#' }
#' 
#' 
#' @export convertSpectra
convertSpectra <- function(x, method = NULL){
  
  if(class(x) == "spectra.list"){x <- as.spectra.matrix(x)}
  
  if(is.null(method)){stop("Please specify a method.")}else{

    if("WN_to_WL" %in% method){colnames(x) <- (10^7/as.numeric(colnames(x))); attr(x, "wave_unit") <- "wavelength"}
    if("WL_to_WN" %in% method){colnames(x) <- (10^7/as.numeric(colnames(x))); attr(x, "wave_unit") <- "wavenumber"}   
        
    if("A_to_TR" %in% method){x <- 10 ^ (-x); attr(x, "measurement_unit") <- "transmittance"}
    if("TR_to_A" %in% method){x <- log10(1/x); attr(x, "measurement_unit") <- "absorbance"}
    
    if("A_to_R" %in% method){x <- 10 ^ (-x); attr(x, "measurement_unit") <- "reflectance"}
    if("R_to_A" %in% method){x <- log10(1/x); attr(x, "measurement_unit") <- "absorbance"}    

    return(x)
  }
}
