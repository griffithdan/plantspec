calibrate <- function(component, spectra, optimal_params = NULL, optimal_model = 1, validation = 'none', training_set = NULL, parallel = FALSE, max_comps = 10){
  
  custom_comp <- max_comps
  
  # test out mean centering
    #component <- component - mean(component)
    #m_spec <- apply(X = spectra,2,mean)
    #spectra <- t(apply(X = spectra,1,function(x){as.numeric(x) - as.numeric(m_spec)}))
  #
  
  # HANDLE OPTIMIZATION RESULTS
  
    if(!is.null(optimal_params)){
      
      regions <- optimal_params$param_subsets
      preproc <- optimal_params$param_preproc
      regions <- regions[[optimal_model]]
      preproc <- preproc[[optimal_model]]
      
      ##spectra <- subsetSpectra(spec=spectra,ranges=regions)
      ##
      ##
      ##for(p in preproc){
      ##
      ##  if(class(spectra)=="spectra.matrix"){spectra <- as.spectra.list(spectra)}
      ##  spectra <- preprocess(spec=spectra,transformation=p) # test to be sure this is the same when there is two... there is a better implementation anyway
      ##
      ##}
      ##
      ### o NA issue
      ##
      ##if("subset_string" %in% names(attributes(spectra))){
      ##  
      ##  subset_string <- attr(spectra,"subset_string")
      ##  spectra[,is.na(subset_string)] <- 0
      ##  
      ##}  
      ############################################
      
      # PREPROCESSING
      
      cur_spec <- spectra
      
      cur_spec_train <- cur_spec[training_set,]
      cur_spec_test <- cur_spec[!(training_set),]

      cur_spec_train <- subsetSpectra(spec=cur_spec_train,ranges=regions)
      cur_spec_test <- subsetSpectra(spec=cur_spec_test,ranges=regions)
        
        #for(p in p_selection){
          
          #if(class(cur_spec_train)=="spectra.matrix"){cur_spec_train <- as.spectra.list(cur_spec_train)}
          cur_spec_train <- preprocess(spec=as.spectra.list(cur_spec_train),transformation=preproc)
      
      if("MSC_reference" %in% names(attributes(cur_spec_train))){
          cur_spec_test <- preprocess(spec=as.spectra.list(cur_spec_test),transformation=preproc,MSC_reference = attr(cur_spec_train,"MSC_reference"))
      }else{
          cur_spec_test <- preprocess(spec=as.spectra.list(cur_spec_test),transformation=preproc)
      }
          
        #}
      
      
      cur_spec <- rbind(cur_spec_train,cur_spec_test)
      if("subset_string" %in% names(attributes(cur_spec_train))){
        attr(cur_spec, "subset_string") <- attr(cur_spec_train, "subset_string")
      }      
      # HERE WE NEED THE ABILITY TO RUN PREPROC USING A SUBSET AS THE REFERENCES (diffs stem only from MSC and D1f/MSC)
      
      # MUST REINSTATE PROPER ORDER, maybe based on names
      
    
      # o NA issue
      
      if("subset_string" %in% names(attributes(cur_spec))){
        
        subset_string <- attr(cur_spec,"subset_string")
        cur_spec[,is.na(subset_string)] <- 0
        
      }   
      
            cur_spec <- cur_spec[row.names(spectra),]
      
      spectra <- cur_spec
      ############################################
      #spectra <- subsetSpectra(spec=spectra,ranges=regions) # Orig
      
    }else{regions <- "full spectrum"; preproc <- "raw spectra"; cur_spec_train <- "none"}
     
  # HANDLE OTHER PARAMETERS
  
    if (validation == "none"){
      val <- "LOO" # because the LOO stats are needed to calculate the optimal number of factors
    }
    if (validation == "LOO"){
      val <- "LOO"
    }
    if (validation == "testset"){
      val <- "none" # because the LOO stats are needed to calculate the optimal number of factors # changed to none to now allow only rmsep    
    }
  
    if (parallel == TRUE){
      nCores <- detectCores() - 1 # leave a core for user
      nCores <- ifelse(nCores<1, yes = 1, no = nCores) # make sure at least 1 core is used
      pls.options(parallel = makeCluster(nCores, type = "PSOCK"))
    }
  
  
  # RUN PLS MODEL
  
    train_data <- data.frame(component=component)
      #class(spectra) <- "matrix"
      #colnames(spectra) <- NULL
      #row.names(spectra) <- NULL

    train_data$spectra <- spectra
    rm(spectra)
    rm(component)
  
    if (validation == "testset"){
      
      pls_mod <- mvr(formula= component ~ spectra,
                     subset = training_set,
                     method = "kernelpls",#"kernelpls", 
                     validation = val,
                     ncomp = custom_comp,#10,
                     data = train_data)
    }else{
      
      pls_mod <- mvr(formula= component ~ spectra,
                     method = "kernelpls",#"kernelpls", 
                     validation = val,
                     ncomp = custom_comp,#10,
                     data = train_data)
    }
  
    if (parallel == TRUE){
      stopCluster(pls.options()$parallel)
    }

  
  if(validation == "LOO"){
  # SELECT THE OPTIMAL NUMBER OF FACTORS
  
        MSECV <- MSEP(pls_mod,estimate="CV")
        PRESS <- data.frame(nfactor=as.numeric(MSECV$comps),press=as.numeric(MSECV$val)*nrow(pls_mod$model))
        PRESS <- PRESS[-1,]
        PRESS$F <- PRESS$press / PRESS[with(PRESS,which.min(press)),"press"]
        PRESS$P <- pf(q=PRESS$F,df1=nrow(pls_mod$model),df2=nrow(pls_mod$model))
        rank <- min(PRESS[with(PRESS,P<=0.75),"nfactor"])
      
      calibration <- list(model = pls_mod,
                          rank = rank,
                          RMSEP = RMSEP(pls_mod,ncomp=rank,estimate="train")[[1]][2],
                          R2_Cal = R2(pls_mod,ncomp=rank,estimate="train")[[1]][2],
                          regions = regions,
                          preproc = preproc)
    
      
      if("MSC_reference" %in% names(attributes(cur_spec_train))){
          calibration$MSC_reference <- attr(cur_spec_train,"MSC_reference")
      }  
      
    calibration$RMSECV = RMSEP(pls_mod,ncomp=rank,estimate="CV")[[1]][2]
    calibration$R2_Val = R2(pls_mod,ncomp=rank,estimate="CV")[[1]][2]
  
  }
  if(validation == "testset"){

  # SELECT THE OPTIMAL NUMBER OF FACTORS
  
        MSEP <- MSEP(pls_mod,estimate="test",newdata=train_data[!(training_set),])
        PRESS <- data.frame(nfactor=as.numeric(MSEP$comps),press=as.numeric(MSEP$val)*nrow(pls_mod$model))#nrow(train_data[!(training_set),]))# # check nrow for trianing
        PRESS <- PRESS[-1,]
        PRESS$F <- PRESS$press / PRESS[with(PRESS,which.min(press)),"press"]
        PRESS$P <- pf(q=PRESS$F,df1=nrow(pls_mod$model),df2=nrow(pls_mod$model))
        rank <- min(PRESS[with(PRESS,P<=0.75),"nfactor"])
      
      calibration <- list(model = pls_mod,
                          rank = rank,
                          RMSEP = RMSEP(pls_mod,ncomp=rank,estimate="test",newdata=train_data[!(training_set),])[[1]][2],
                          R2_Cal = R2(pls_mod,ncomp=rank,estimate="train")[[1]][2],
                          regions = regions,
                          preproc = preproc,
                          spectra = spectra) 
      if("MSC_reference" %in% names(attributes(cur_spec_train))){
          calibration$MSC_reference <- attr(cur_spec_train,"MSC_reference")
      }  
    
    calibration$R2_Val <- R2(pls_mod,ncomp=rank,estimate="test",newdata= train_data[!(training_set),])[[1]][2]
    calibration$training_set <- training_set
  }
  
  calibration$data <- train_data
  
  class(calibration) <- "PLScalibration"
  return(calibration)
  
}