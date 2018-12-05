#' Function to subset regions of spectra.
#' 
#' This function accepts an object containing spectra and subsets it to a
#' specified region(s).
#' 
#' 
#' @param spec An object of class \code{spectra.matrix} to be subset.
#' @param ranges A two column matrix, each row contains max and min range
#' values. Alternatively, a list, where each element is a vector of length 2,
#' specifying the range (max/min) of a spectral region to select. Should be in 
#' the same units as your spectra (e.g., wavenumbers).
#' @return Returns an object of class \code{spectra.list}.
#' @author Daniel M Griffith
#' @keywords manipulation
#' @examples
#' 
#' \dontrun{
#' data(shootout)
#' s_mat <- matrix(data = c(2000,1750,1000,500), nrow = 2, ncol = 2, byrow = T)
#' sub_data <- subsetSpectra(spec = shootout_scans, ranges = s_mat)
#' 
#' par(mfrow=c(1,2))
#' plot(shootout_scans)
#' plot(sub_data)
#' }
#' 
#' @export subsetSpectra
subsetSpectra <- function(spec, ranges){
  
    wavenumbers <- as.numeric(colnames(spec))
    wn_ind <- rep(FALSE,times=length(wavenumbers))
    
    if(is.list(ranges)){
        ranges <- lapply(ranges,function(x){sort(x, decreasing = TRUE)})
        ranges <- matrix(data = unlist(ranges), ncol = 2, byrow = TRUE)
    }

    for(r in 1:nrow(ranges)){
      
      range <- ranges[r,]
      wn_ind[wavenumbers <= range[1] & wavenumbers > range[2]] <- TRUE
      
    }
    
    spec[,!(wn_ind)] <- NA #0 #0 set
    
    subset_id <- rep(NA,length(wn_ind))
    cur_group <- 1
    subset_id[1] <- 1
    for (i in 2:length(wn_ind)){
      if(wn_ind[i] == wn_ind[i-1]){
        subset_id[i] <- cur_group
      }else{
        cur_group <- cur_group + 1
        subset_id[i] <- cur_group
      }    
    }
    
    subset_id <- subset_id*wn_ind
    
    orig_ids <- unique(subset_id)
    new_ids <- 0:(length(orig_ids)-1)
    subset_id <- new_ids[match(subset_id,orig_ids)]
    subset_id[subset_id==0] <- NA
    
    subset_string <- subset_id
    
    attr(spec, "subset_string") <- subset_string
    
    #spec <- spec[,wn_ind] #original
    return(spec)
  
}
