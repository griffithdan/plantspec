plotSpectra <- function(x, col = "black",type = NULL, xlim = NULL, ylim = NULL, add = FALSE, ...){
  
  if(class(x) == "spectra.matrix"){
    x <- as.spectra.list(x)
  }
  
  if(!(exists("add"))|if(exists("add")){ifelse(add==FALSE,TRUE,FALSE)}){
        if(exists("add")){rm(add)}

    if(is.null(ylim)){
      ymax <- max(unlist(lapply(X = x,FUN = function(s){max(s$measurement,na.rm = T)})),na.rm = T)
      ymin <- min(unlist(lapply(X = x,FUN = function(s){min(s$measurement,na.rm = T)})),na.rm = T) 
      ylims <- c(ymin,ymax)
    }else{ylims = ylim; ylim = NULL}
    if(is.null(xlim)){
      xmax <- max(unlist(lapply(X = x,FUN = function(s){max(s$wave_value,na.rm = T)})),na.rm = T)
      xmin <- min(unlist(lapply(X = x,FUN = function(s){min(s$wave_value,na.rm = T)})),na.rm = T) 
      xlims <- rev(c(xmin,xmax))
    }else{xlims = xlim; xlim = NULL}
    if(is.null(type)){
    linetype = "l"
    }else{linetype = type; type = NULL}
  
    yvals <- x[[1]]$measurement
    xvals <- x[[1]]$wave_value
    plot(yvals ~ xvals,ylim=ylims,xlim=xlims,type = linetype,
        ylab = ifelse(attr(x[[1]],"measurement_unit") == "absorbance", "Absorbance", "Transmittance"), 
        xlab = ifelse(attr(x[[1]],"wave_unit") == "wavenumber","Wavenumber","Wavelength"),
        col = col, ...)# reg mode
    #plot(yvals ~ xvals,type = "l",ylab = "Absorbance", xlab = "Wavenumber",col = col,main=names(x)[1]) # checkmode
    
    #readline()
    if(length(x)>1){
      for (i in 2:length(x)){
        
        yvals <- x[[i]]$measurement
        xvals <- x[[i]]$wave_value
        points(yvals ~ xvals,type = linetype, col = col, ...) # reg mode
        
        # check mode
          #plot(yvals ~ xvals,type = "l",ylab = "Absorbance", xlab = "Wavenumber",col = col,main=names(x)[i]) # a whole section of scans is backwards????
    
          #readline()
      }}
  }else{if(add==T){
    
    rm(add)
    
    if(is.null(type)){
    linetype = "l"
    }else{linetype = type; type = NULL}
  
    yvals <- x[[1]]$measurement
    xvals <- x[[1]]$wave_value
    points(yvals ~ xvals,type = linetype,
        col = col, ...)# reg mode
    #plot(yvals ~ xvals,type = "l",ylab = "Absorbance", xlab = "Wavenumber",col = col,main=names(x)[1]) # checkmode
    
    #readline()
    if(length(x)>1){
      for (i in 2:length(x)){
        
        yvals <- x[[i]]$measurement
        xvals <- x[[i]]$wave_value
        points(yvals ~ xvals,type = linetype, col = col, ...) # reg mode
        
        # check mode
          #plot(yvals ~ xvals,type = "l",ylab = "Absorbance", xlab = "Wavenumber",col = col,main=names(x)[i]) # a whole section of scans is backwards????
    
          #readline()
      }}
    
  }}
}