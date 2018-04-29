#' Function to read spectra.
#' 
#' This function accepts a vector of file paths specifying files that contain
#' spectra. The function incorporates these spectra into a single
#' \code{spectra.matrix} object. The function supports .dpt, .txt, opus binary
#' (e.g., .0), and .spc files. Files import for .spc files is done using the
#' package \code{hpyerSpec} and import of opus format is done using the package
#' \code{soil.spec}. These formats are auto detected. Feel free to
#' request/suggest file formats.
#' 
#' 
#' @param filelist A vector (e.g., the output of \code{list.files(full.names =
#' TRUE)}) containing the file paths for spectra to read.
#' @param wave_unit "wavenumber" or "wavelength".
#' @param measurement_unit "absorbance" or "transmittance".
#' @return Returns an object of class \code{spectra.matrix}.
#' @author Daniel M Griffith
#' @references Claudia Beleites and Valter Sergo: hyperSpec: a package to handle
#'   hyperspectral data sets in R', R package version 0.99-20171005. 
#'   http://hyperspec.r-forge.r-project.org
#'   Andrew Sila, Tomislav Hengl and Thomas Terhoeven-Urselmans (2014). 
#'   soil.spec: Soil Spectroscopy Tools and Reference Models. R package version 
#'   2.1.4. https://CRAN.R-project.org/package=soil.spec
#' @keywords read/write/fileio
#' @examples
#' 
#' 
#' ## MY_PATH = is a text string identifying the folder with the spectra 
#' # specs <- readSpectra(filelist = list.files("MY_PATH", full.names = T))
#' 
#' 
#' @export readSpectra
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
    colnames(spec) <- hyperSpec::read.spc(filename = filelist[[1]])@wavelength
    row.names(spec) <- basename(filelist)
    
      attr(spec, "wave_unit") <- wave_unit
      attr(spec, "measurement_unit") <- measurement_unit
      class(spec) <- "spectra.matrix"
    
  }

  return(spec)

}
