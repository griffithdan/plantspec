#' Function to convert data to an object of class \code{spectra.list}.
#' 
#' Convert data to an object of class \code{spectra.list} from an object of
#' class \code{spectra.matrix}.
#' 
#' 
#' @param x Object of class \code{spectra.matrix}.
#' @param ...  Additional args.
#' @return Returns an object of class \code{spectra.list}.
#' @author Daniel M Griffith
#' @keywords manipulation
#' @examples
#' 
#' 
#' #data(shootout)
#' #scans <- as.spectra.list(shootout_scans)
#' 
#' 
#' @export as.spectra.list
as.spectra.list <- function(x, ...){
  
  if( class(x)=="spectra.list"){return(x)}else{
    newlist <- apply(X=x,1,FUN=list)
    
    wave_unit <- attr(x, "wave_unit")
    measurement_unit <- attr(x, "measurement_unit")
    
    newlist <- lapply(X=newlist,FUN=function(x){tmp <-data.frame(wave_value=as.numeric(names(unlist(x))),measurement=unlist(x));
                                                attr(tmp, "wave_unit") <- wave_unit;
                                                attr(tmp, "measurement_unit") <- measurement_unit;
                                                tmp})

    class(newlist) <- "spectra.list"
    
    if("subset_string" %in% names(attributes(x))){
      
      attr(newlist,"subset_string") <- attr(x,"subset_string")
      
    }
    
    return(newlist)
  }
}
