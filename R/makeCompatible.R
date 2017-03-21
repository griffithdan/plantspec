makeCompatible <- function(x, ref){
  
  if(class(x) == "spectra.list"){x <- as.spectra.matrix(x)}
  if(class(ref) == "spectra.list"){ref <- as.spectra.matrix(ref)}
  
  if(attr(x,"wave_unit") != attr(ref,"wave_unit")){
    ref_wu <- attr(ref,"wave_unit")
    if(ref_wu == "wavelength"){x <- convertSpectra(x = x, method = "WN_to_WL")}
    if(ref_wu == "wavenumber"){x <- convertSpectra(x = x, method = "WL_to_WN")}
  }
  if(attr(x,"measurement_unit") != attr(ref,"measurement_unit")){
    ref_mu <- attr(ref,"measurement_unit")
    if(ref_wu == "absorbance"){x <- convertSpectra(x = x, method = "TR_to_A")}
    if(ref_wu == "transmittance"){x <- convertSpectra(x = x, method = "A_to_TR")}     
  }  
  
  ref_wavebands <- as.numeric(colnames(ref))
  x_wavebands <- as.numeric(colnames(x))
  
  new_x <- apply(X = x, MARGIN = 1, FUN = function(meas){
            approx(x = x_wavebands, 
                   y = meas,
                   xout = ref_wavebands)$y
           })
  new_x <- matrix(data = unlist(new_x),nrow = nrow(x), ncol = ncol(ref), byrow = T)

  colnames(new_x) <- ref_wavebands
  row.names(new_x) <- row.names(x)
  attr(new_x,"wave_unit") <- attr(ref,"wave_unit")
  attr(new_x,"measurement_unit") <- attr(ref,"measurement_unit")
  class(new_x) <- "spectra.matrix"
  
    return(new_x)
}