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