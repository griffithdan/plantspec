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