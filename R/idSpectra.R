#' Identify spectra in a plot.
#' 
#' This function accepts spectra (the same ones given to \code{plot()}), allows
#' the user to click a scan, then returns the scan name in the console and
#' colors the selected scan red. This is useful is you are using the 
#' \code{plotSpectra()} with \code{base_plot == TRUE}, although the \code{plotly}
#' interactive version is preferable and already interactive.
#' 
#' 
#' @param spec An object of class \code{spectra.list} or \code{spectra.matrix}.
#' @return Returns an the name of the selected scan.
#' @author Daniel M Griffith
#' @keywords visualization
#' @examples
#' 
#' 
#' #data(shootout)
#' #plot(shootout_scans, base_plot == TRUE)
#' #idSpectra(shootout_scans)
#' 
#' @export
#' 
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
