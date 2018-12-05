#' Prediction method for \code{PLScalibration}.
#' 
#' This function accepts a PLS model of class \code{PLScalibration} and new
#' data as a \code{spectra.matrix}. It uses the pls::predict.mvr method for
#' predicting new data and also provides a mahalanobis distance (to the
#' multivariate center of the calibration dataset) for each new sample.
#' In order to do conduct the prediction, the new data are subsetted with
#' \code{subsetSpectra()} and then preprocessed with \code{preprocess()}, 
#' according to the optimal transformation information stored in the PLS 
#' calibration object.
#' 
#' 
#' @param object An object of class \code{PLScalibration} containing the model.
#' @param newdata New data (scans) in the same units and resolution as the
#' spectra used to fit the PLS model. These spectra should be raw (without
#' preprocessing) if the spectra used to fit the model were not manually
#' manipulated before the use of \code{optimizePLS()} or \code{calibrate()}.
#' \code{predictPLS()} will automaticall conduct the appropriate preprocessing
#' and spectral subsetting if these steps were conducted within
#' \code{calibrate()}.
#' @param ... Additional args.
#' @return Returns named a vector of predicted values with the following
#' attributes:\cr "mahalanobis" - The mahalanobis distance of each new spectrum
#' from the calibration mean.\cr "mahalanobis_threshold" - The maximum
#' mahalanobis distance in the calibration dataset.\cr "outlier" - Logical.
#' TRUE is "mahalanobis" is greater than "mahalanobis_threshold."
#' @author Daniel M Griffith
#' @keywords calibration
#' @examples
#' 
#' \dontrun{
#' data(shootout)
#' data(N_cal_shootout)
#' 
#' shootout_scans <- convertSpectra(x = shootout_scans, method = "WL_to_WN")
#' 
#' N_predicted <- predict(object = N_cal_shootout, newdata = shootout_scans)
#' hist(N_predicted)
#' }
#' 
#' @export predictPLS
predictPLS <- function(object, newdata, ...){
    
  model <- object$model
  regions <- object$regions
  preproc <- object$preproc

    datasub <- subsetSpectra(spec=newdata,ranges=regions)
    datapro <- preprocess(spec = datasub,transformation = preproc, MSC_reference = if("MSC_reference" %in% names(object)){object$MSC_reference}else{NULL})
    datapro[is.na(datapro)]<-0
    prediction <- predict(object=model,newdata=datapro,ncomp=object$rank,type="response")
    
    
    # MAHALANOBIS
  
      #datapro # needs to be converted into latent vector space
      #latvec # need to find the latent vectors from the model calibration set
  
      #mahalanobis(x = spec,center = colMeans(latvec),cov = diag(ncol(latvec)),tol=1e-100)
  
      model_latent_vectors <- scores(object = model) # scores ARE SAME LENGTH AS NUMBER OF samples AND THEREFOR THIS IS WHAT I THINK WILL BE USED IN THE MAHALANOBIS
      class(model_latent_vectors) <- "matrix"
      model_latent_vectors <- model_latent_vectors[,c(1:object$rank)]
  
      new_data_latent_vectors <- predict(object=model,newdata=datapro,ncomp=c(1:object$rank),type="scores")
    
      new_data_mahal <- sqrt(mahalanobis(x = new_data_latent_vectors,center = colMeans(model_latent_vectors),cov = diag(ncol(model_latent_vectors))))#,tol=1e-100)
      max_mahal <- max(sqrt(mahalanobis(x = model_latent_vectors,center = colMeans(model_latent_vectors),cov = diag(ncol(model_latent_vectors)))))#,tol=1e-100)
      
############## manual way to calc the predictions
#            nobs <- dim(datapro)[1]
#            B <- rowSums(coef(model, comps = c(1:object$rank)), dims = 2)
#            B0 <- model$Ymeans - model$Xmeans %*% B
#            pred <- datapro %*% B + rep(B0, each = nobs)
########################
  
  ############################################
  
  prediction <- prediction[,1,]
  #prediction <- as.numeric(prediction)
  attr(prediction, "mahalanobis") <- new_data_mahal
  attr(prediction, "mahalanobis_threshold") <- max_mahal
  attr(prediction, "outlier") <- new_data_mahal > max_mahal

    return(prediction)
    
}