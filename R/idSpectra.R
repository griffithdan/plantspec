idSpectra <- function(spec){
  
  if(class(spec) == "spectra.list"){
    spec <- as.spectra.matrix(spec)
  }
  
  xvals <- rep(x = as.numeric(colnames(spec)), each = nrow(spec))
  yvals <- as.vector(spec)
  val_names <- rep(x = row.names(spec), times = ncol(spec))
  
  val <- identify(x = xvals, y = yvals, plot=FALSE, n = 1)

  mattemp <- cbind(xvals,yvals)
  row.names(mattemp) <- val_names
  
  k <- arrayInd(val, dim(mattemp))
  
  selected <- rownames(mattemp)[k[,1]]
  
  selected_spec <- spec[selected,]
  
  lines(y = selected_spec, x = as.numeric(colnames(selected_spec)), col = "red")
  
  return(selected)
  
}