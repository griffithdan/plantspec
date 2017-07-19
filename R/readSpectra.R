readSpectra <- function(filelist, wave_unit = "wavenumber", measurement_unit = "absorbance", ...){

  f_ext <- file_ext(filelist[1])
  
  if(f_ext %in% c("dpt","txt")){
    spec <- lapply(X=filelist,FUN=function(x){readSpectrum(x, wave_unit = wave_unit, measurement_unit = measurement_unit, ...)[[1]]})
    names(spec) <- basename(filelist)
    class(spec) <- "spectra.list"
    spec <- as.spectra.matrix(spec)
  }
  if(f_ext %in% as.character(c(0:9))){
    spec <- suppressMessages(read.opus(file.name = as.list(filelist)))
    spec <- spec@data@ab
    row.names(spec) <- basename(filelist)
    spec$SAMPLEID <- NULL
    colnames(spec) <- as.numeric(gsub(pattern = "n", replacement = "", x = colnames(spec)))
    spec <- as.matrix(spec)
    
      attr(spec, "wave_unit") <- wave_unit
      attr(spec, "measurement_unit") <- measurement_unit
      class(spec) <- "spectra.matrix"
  }
  if(f_ext %in% c("spc")){
    spec <- lapply(X=filelist,FUN=function(x){read.spc(filename = x)$spc})
    spec <- do.call('rbind',spec)
    colnames(spec) <- read.spc(filename = filelist[[1]])@wavelength
    row.names(spec) <- basename(filelist)
    
      attr(spec, "wave_unit") <- wave_unit
      attr(spec, "measurement_unit") <- measurement_unit
      class(spec) <- "spectra.matrix"
    
  }

  return(spec)

}