subdivideDataset <- function(spectra, component = NULL, type = "validation", p = 0.2, method = "KS", seed.set = NULL, output = "logical"){
  
    N <- round(p * nrow(spectra))
    
    if (is.null(row.names(spectra))){row.names(spectra)<-c(1:nrow(spectra))}
  
    # METHOD SELECTION
    
    if (method == "random"){
      
      if (exists("seed.set")){set.seed(seed.set)}
      selection <- sample(x = c(1:nrow(spectra)), size = N, replace = FALSE)
      selection <- row.names(spectra[selection,])
      names(selection) <- selection
      
    }else{
    
      if (method == "KS"){
        dmat <- dist(spectra, method = "euclidean")
        dmat <- as.matrix(dmat)
      }
      if (method == "PCAKS"){
        dmat <- dist(prcomp(spectra, scale = T)$x[,1:2], method = "euclidean")
        dmat <- as.matrix(dmat)
      }
      if (method == "SPXY"){
        dmat_spec <- dist(spectra, method = "euclidean")
        dmat_spec <- as.matrix(dmat_spec)
        dmat_spec <- dmat_spec / max(dmat_spec)
        dmat_y <- dist(component, method = "euclidean")
        dmat_y <- as.matrix(dmat_y)
        dmat_y <- dmat_y / max(dmat_y)        
        dmat <- dmat_spec + dmat_y      
      }
      if (method == "MDKS"){
        augment_mat <- cbind(component,spectra)
        dmat <- mahalanobis.dist(data.x = prcomp(augment_mat, scale = T)$x[,1:round(0.2 * nrow(spectra))])
      }
    
      seed <- which(dmat == max(dmat), arr.ind=TRUE)
      selection <- seed[1:2,1]
      
      # CALIBRATION SELECTION
      if (type == "calibration"){
        for (i in 3:N) {
          candidate_dist <- apply(dmat[,selection],1,function(x){x[which(x==min(x))]})
          candidate_dist[selection] <- NA
          selection <- c(selection, which.max(candidate_dist))
        } 
      }
      # VALIDATION SELECTION
      if (type == "validation"){
        training_set <- selection
        temp_dmat <- dmat
        temp_dmat[selection,] <- NA
        temp_dmat[,selection] <- NA
        seed2 <- which(dmat == max(temp_dmat, na.rm = T), arr.ind=TRUE)
        rm(temp_dmat)
        test_set <- seed2[1:2,1]
        train_test_switch <- 0 # 0 = train
        
        while (length(test_set) < N) {# STEP 3
          all_selected <- c(training_set, test_set)
          candidate_dist <- apply(dmat[,all_selected],1,function(x){x[which(x==min(x))]})
          candidate_dist[all_selected] <- NA
          
          if (train_test_switch == 0){training_set <- c(training_set, which.max(candidate_dist));train_test_switch <- 1}else{test_set <- c(test_set, which.max(candidate_dist));train_test_switch <- 0}
        }
        all_selected <- c(training_set, test_set)
        #if (length(all_selected) < nrow(spectra)){
        #  remaining_samples <- row.names(dmat)[!(row.names(dmat) %in% names(all_selected))]
        #  remaining_names <- remaining_samples
        #  remaining_samples <- match(remaining_samples, table = row.names(dmat))
        #  names(remaining_samples) <- remaining_names
        #  training_set <- c(training_set,remaining_samples)
        #}
      
      selection <- test_set
        
      }
    
    }
  
    if (output == "logical"){
      selection <- row.names(spectra) %in% names(selection)
    }
    if (output == "names"){}
    return(selection)

}