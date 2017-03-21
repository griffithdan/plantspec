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