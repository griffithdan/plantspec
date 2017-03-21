plotPLScalibration <- function(x, pch = 21, bg = "gray", ncomp = NULL, plottype = "prediction", ...){
  
  if(plottype == "prediction"){
    plot(x$model, pch = pch, bg = bg, ncomp = ifelse(is.null(ncomp),x$rank,ncomp), plottype = "prediction", ...)
    abline(a = 0, b = 1)
  }else{
    plot(x$model, plottype = plottype, ...)      
  }
  
}