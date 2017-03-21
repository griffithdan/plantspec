subsetSpectra <- function(spec,ranges){
  
    wavenumbers <- as.numeric(colnames(spec))
    wn_ind <- rep(FALSE,times=length(wavenumbers))
    
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