#' extract methods
#' 
#' extract methods
#' 
#' @name [.spectra.matrix
#' @aliases extract-methods [.spectra.matrix
#' @param x Object of class \code{spectra.matrix}.
#' @param ...  Additional args.
#' @return Returns an object of class \code{spectra.matrix}.
#' @author Daniel M Griffith
#' @keywords manipulation
#' 
NULL

#`[.spectra.matrix` <- function(x, i, j, ...){
#  
#      #r <- NextMethod("[")
#      #mostattributes(r) <- attributes(x)
#      #r
#  
#    out <- unclass(x)
#    out <- out[i, j, drop=FALSE]
#    
#    #mostattributes(out) <- attributes(x)
#    
#    attr(out, "wave_unit") <- attr(x, "wave_unit")
#    attr(out, "measurement_unit") <- attr(x, "measurement_unit")
#    
#    if("subset_string" %in% attributes(x)){
#      attr(out, "subset_string") <- attr(x, "subset_string")[j]
#    }
#
#    class(out) <- class(x)    
#    out
#  }


#`[.spectra.matrix` <- function(x,i,j, ...){
`[.spectra.matrix` <- function(x, ...){
  
#source: https://stat.ethz.ch/pipermail/r-help/2006-July/109148.html  
  
  wu <- attr(x, "wave_unit")
  mu <- attr(x, "measurement_unit")
  
  atr <- attributes(x)
  atr.names <- names(atr)
  sda <- options()$'spectra.matrix.attributes'
  sda.match <- match(atr.names, sda)
  sda.match <- sda.match[!is.na(sda.match)]
  
  x <- NextMethod("[",drop = FALSE)
  ## assign source.data.attributes to result
  if(length(sda.match))
    for (i in sda.match) attr(x, sda[i]) <- atr[[sda[i]]]
  ## assign class source.data to result
  class(x) <- c('spectra.matrix', attr(x, "class")[attr(x, "class") != "spectra.matrix"]) 
  
    attr(x, "wave_unit") <- wu
    attr(x, "measurement_unit") <- mu
  
  x
}


# need some methods for spec.lists, and for as!