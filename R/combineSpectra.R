combineSpectra <- function(x, y){
  
  if(class(x) == "spectra.list"){x <- as.spectra.matrix(x)}
  if(class(y) == "spectra.list"){y <- as.spectra.matrix(y)}
  
  if(attr(x,"wave_unit") != attr(y,"wave_unit")){stop("x and y are not in the same wave unit. Try convertSpectra().")}
  if(attr(x,"measurement_unit") != attr(y,"measurement_unit")){stop("x and y are not in the same measurement unit. Try convertSpectra().")}

  if(suppressWarnings(any(as.numeric(colnames(x)) != as.numeric(colnames(y))))){stop("x and y do not have the same spectral resolution or extent. Try makeCompatible().")}

  x_wavebands <- as.numeric(colnames(x))
  
  new_x <- rbind(x,y)    
                        
  attr(new_x,"wave_unit") <- attr(x,"wave_unit")
  attr(new_x,"measurement_unit") <- attr(x,"measurement_unit")
  class(new_x) <- "spectra.matrix"
  
    return(new_x)
}