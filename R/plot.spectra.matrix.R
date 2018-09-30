#' Function to plot spectra.
#' 
#' This function accepts spectra in a \code{spectra.list} or
#' \code{spectra.matrix} object and plots them.
#' 
#' 
#' @param x An object of class \code{spectra.list} or \code{spectra.matrix}
#' containing the spectra to plot.
#' @param col see par.
#' @param type see par.
#' @param xlim see par.
#' @param ylim see par.
#' @param add see par.
#' @param base_plot Default is FALSE. If FALSE, plots spectra as an interactive \code{plotly} object.
#' @param ... Additional args.
#' @author Daniel M Griffith
#' @keywords visualization
#' @examples
#' 
#' 
#' #data(shootout)
#' #plot(shootout_scans)
#' 
#' @export plot.spectra.matrix
#' @export 
plot.spectra.matrix <- function(x, col = "black",type = NULL, xlim = NULL, ylim = NULL, base_plot = FALSE, ...){
  
  plotSpectra(x = x, col = col, type = type, xlim = xlim, ylim = ylim, base_plot = base_plot, ...)

}
