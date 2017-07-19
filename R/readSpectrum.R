readSpectrum <- function(filename, wave_unit = "wavenumber", measurement_unit = "absorbance", ...){
  if(missing(sep)) {
        sep=""
    }
  spec <- read.table(file=filename,header=F,quote="", ...)
  colnames(spec) <- c("wave_value","measurement")
  spec_list <- list()
    attr(spec, "wave_unit") <- wave_unit
    attr(spec, "measurement_unit") <- measurement_unit
  spec_list[[1]] <- spec
  class(spec_list) <- "spectra.list" # need to add support here as a list
  return(spec_list)  
}