#' Function to average spectra.
#' 
#' This function accepts spectra, averages them, and returns the results. This
#' function is a simple wrapper for the \code{aggregate()} function, made to work
#' smoothly with spectra.
#' 
#' 
#' @param spec An object of class \code{spectra.list} or \code{spectra.matrix}.
#' @param by Either (i) a vector of length \code{nrow(spec)} that specifies
#' which combinations of spectra to average together, or (ii) a single numeric
#' value specifying how many scans to combine, in order. Using the former
#' option, the mean spectra will be named according to the values of \code{by}. For
#' the latter option, they will be named in order numerically.
#' @return Returns an object of class \code{spectra.matrix}.
#' @author Daniel M Griffith
#' @keywords manipulation
#' @examples
#' 
#' \dontrun{
#' data(shootout)
#' by_vector <- gsub(pattern = "_.*",replacement = "", 
#'  x = row.names(shootout_scans)) # average everything to one scan
#' mean_scans <- averageSpectra(spec = shootout_scans, by = by_vector)
#' #or...
#' mean_scans <- averageSpectra(spec = shootout_scans, by = 3) 
#' # average scans in groups of 3
#' par(mfrow=c(1,2))
#' plot(shootout_scans)
#' plot(mean_scans)
#' }
#' 
#' @export averageSpectra
averageSpectra <- function(spec = NULL, by = NULL){ # this is a glorified aggregate function, but is wrapped up to work easy for spectra
  
  if(is.null(by)){by <- row.names(spec)}
  
  if(is.data.frame(spec)){spec <- list(spec)}
  if(class(spec) == "spectra.list"){spec <- as.spectra.matrix(spec)}
  
  cnames <- colnames(spec)
  
    if("subset_string" %in% names(attributes(spec))){
      
      subset_string <- attr(spec,"subset_string")
      subset_unique <- na.omit(unique(subset_string))
      
    }
  
  if(is.numeric(by) & length(by)==1){
    bins <- floor(nrow(spec)/by)
    by <- rep(c(1:bins), each = by)
    by <- c(by,rep(c(bins+1),nrow(spec) - length(by)))
    
  }
  
  
  # BEGIN AVERAGING
  
###################################################################
 
  #data(shootout)
  # spec <- shootout_scans
  # by <- row.names(spec)
  
  class(spec) <- "matrix"
  
  mean_specs <- aggregate(x = spec,by = list(by),FUN = function(x){mean(x = x, na.rm = T)})
  row.names(mean_specs) <- mean_specs$Group.1
  mean_specs$Group.1 <- NULL
  
  mean_specs <- as.spectra.matrix(mean_specs)
  
###################################################################
  colnames(mean_specs) <- cnames

    if("subset_string" %in% names(attributes(spec))){

      attr(mean_specs, "subset_string") <- subset_string
      
    }

    if("wave_unit" %in% names(attributes(spec))){

      attr(mean_specs, "wave_unit") <- attr(spec, "wave_unit")
      
    }

    if("measurement_unit" %in% names(attributes(spec))){

      attr(mean_specs, "measurement_unit") <- attr(spec, "measurement_unit")
      
    }

  return(mean_specs)
}
