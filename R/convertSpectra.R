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