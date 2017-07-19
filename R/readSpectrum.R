readSpectrum <- function(filename, wave_unit = "wavenumber", measurement_unit = "absorbance", ...){
    args <- list(sep="", file=filename,header=F,quote="") # specify defaults here
    inargs <- list(...)
    args[names(inargs)] <- inargs
    
  spec <- do.call(read.table, args)
  colnames(spec) <- c("wave_value","measurement")
  spec_list <- list()
    attr(spec, "wave_unit") <- wave_unit
    attr(spec, "measurement_unit") <- measurement_unit
  spec_list[[1]] <- spec
  class(spec_list) <- "spectra.list" # need to add support here as a list
  return(spec_list)  
}