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