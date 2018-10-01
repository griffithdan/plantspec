#' Selects a subset of a multivariate (e.g., spectral) dataset.
#' 
#' This function accepts spectra in a \code{spectra.list} or
#' \code{spectra.matrix} object and selects a subset of that dataset.
#' Importantly, the function can be set to select either a calibration or
#' validation subset. These are fundamentally different. When you select a
#' calibration dataset the intention is to choose a representative subset of
#' all spectral data on which to perform wet lab analysis. However, when
#' selecting a subset of samples (which already have wet lab analysis) in order
#' to validate a model, it is important that both the validation (test set) and
#' and calibration (training set) are representative--otherwise, the
#' calibration model will be fit to well sampled spectral space but validated
#' on outlying points. The calibration selection uses the Kennard-Stone
#' algorithm whereas the validation selection uses the Duplex algorithm, which
#' is a modification the original author's proposed. Finally, this function can
#' also perform calibration or validation selection in one of five distinct
#' methods (see the method parameter for details).
#' 
#' 
#' @param spectra An object of class \code{spectra.list} or
#' \code{spectra.matrix} containing the spectra to write.
#' @param component Method "SPXY" and "MDKS" incorporate Y-value data in subset
#' selection. If using one of these two methods, a vector of Y data should be
#' provided here.
#' @param type One of "calibration" or "validation" depending on the type of
#' subset required.
#' @param p The proportion of the dataset to select as the "calibration" or
#' "validation" group.
#' @param method The desired method. Selected from:\cr "KS" - Standard,
#' Kennard-Stone selection. When \code{type = validation} the performs Duplex
#' selection.\cr "PCAKS" - Selection is performed on the principal components
#' from a PCA of the spectra.\cr "SPXY" - Selection occurs on both X (spectra)
#' and Y (component) data with equal weighting.\cr "MDKS" - Mahalanobis
#' distance is used instead of euclidean distances. Selection occurs on both X
#' (spectra) and Y (component) data with equal weighting.\cr "random" - Simple
#' random selection, regardless of multivariate distribution.
#' @param seed.set A single numeric value. If method is "random" then you can
#' set the seed so that the same selection is produced each time.
#' @param output One of "logical" or "names." If "logical" then the function
#' will return a logical vector where TRUE values are the selected samples. If
#' "names" then the names of the selected spectra are returned.
#' @return A vector. Depending on \code{output}, either a logical of list of
#' names indicating selected spectra.
#' @author Daniel M Griffith
#' @references Kennard, R. W. and Stone, L. A. (1969) Computer aided design of
#' experiments. Technometrics, 11, 137-148.
#' 
#' Galvao, R., Araujo, M., Jose, G., Pontes, M., Silva, E. & Saldanha, T.
#' (2005). A method for calibration and validation subset partitioning.
#' Talanta, 67, 736–740.
#' 
#' Saptoro, Agus; Tadé, Moses O.; and Vuthaluru, Hari (2012) "A Modified
#' Kennard-Stone Algorithm for Optimal Division of Data for Developing
#' Artificial Neural Network Models," Chemical Product and Process Modeling:
#' Vol. 7: Iss. 1, Article 13. DOI: 10.1515/1934-2659.1645
#' 
#' Snee, R.D., 1977. Validation of regression models: methods and examples.
#' Technometrics, 19, 415-428.
#' @keywords manipulation
#' @examples
#' 
#' 
#' #data(shootout)
#' #val_set <- subdivideDataset(spectra = shootout_scans, type = "validation", method = "KS")
#' #table(val_set)
#' 
#' 
#' @export subdivideDataset
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
        dmat <- mahalanobis.dist(data.x = prcomp(augment_mat, scale = T)$x[,1:round(0.2 * ncol(spectra))])
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
