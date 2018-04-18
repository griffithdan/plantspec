#' Function to write spectra.
#' 
#' This function accepts spectra in a single \code{spectra.list} or
#' \code{spectra.matrix} object and writes those spectra to separate files.
#' Currently this function only writes ".dpt" files.
#' 
#' 
#' @param spectra An object of class \code{spectra.list} containing the spectra
#' to write.
#' @param filelist A vector of full file names, in the same order as the
#' elements of \code{spectra}.
#' @param path A string specifying a directory, where all spectra will be
#' written using \code{names(spectra)} as file names. Not used with
#' \code{filelist} - ignored if \code{filelist} is specified.
#' @return Writes spectra to files.
#' @author Daniel M Griffith
#' @keywords read/write/fileio
#' @examples
#' 
#' 
#' # data(shootout)
#' # writeSpectra(spectra = shootout_scans, path = "MY_PATH")
#' 
#' 
#' @export writeSpectra
writeSpectra <- function(spectra,filelist=NULL,path=NULL){

  if(class(spectra) == "spectra.matrix"){spectra <- as.spectra.list(spectra)}
  
  buildlist <- filelist
  if(is.null(filelist)){
    buildlist <- names(spectra)
  }
  if(!is.null(path)&is.null(filelist)){
    buildlist <- paste(path,buildlist,sep="/")  
  }
  filelist <- buildlist
  
  if(!(grepl(x = filelist[1], pattern = "\\.dpt$"))){filelist<-paste(filelist,".dpt",sep="")}
  
  for(spec in 1:length(spectra)){
    writeSpectrum(x = spectra[[spec]],filename = filelist[spec])
  }
  
}
