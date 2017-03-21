optimizePLS <- function(component, spectra, training_set = NULL, parallel = FALSE, region_list = NULL, preprocessing_list = NULL, max_comps = 10){

    # test out mean centering # ACTUALLY THIS SHOULD COME LAST
  #component <- component - mean(component[training_set])
  #m_spec <- apply(X = spectra[training_set,],2,mean)
  #spnames <- colnames(spectra)
  #spectra <- as.spectra.matrix(t(apply(X = spectra,1,function(x){as.numeric(x) - as.numeric(m_spec)})))
  #colnames(spectra) <- spnames
  #
    
  optimization_results <- data.frame()
  param_subsets <- list()
  param_preproc <- list()
  
  # CALCULATE ALL 1:N PERMUTATIONS REGIONS AND PREPROCESSING STEPS
  
    # REGIONS
      
      if (is.null(region_list)){
        regions <- list(c(9400,7500),
                        c(7500,6100),
                        c(6100,5450),
                        c(5450,4600),
                        c(4600,4250)) # different format than subsetSpectra... should make the same for consistency
      }else{
        stopifnot(is.list(region_list))
        regions <- region_list
      }
  
    # PREPROCESSING
  
      if (is.null(preprocessing_list)){
        preprocessing <- list("RAW",
                              "D1f",
                              "D2f",
                              "COE",
                              "SLS",
                              "SNV",
                              "MMN",
                              "MSC",
                              c("D1f","SLS"),
                              c("D1f","SNV"),
                              c("D1f","MSC"))

      }else{
        stopifnot(is.list(preprocessing_list))
        preprocessing <- preprocessing_list
      }
      if (length(preprocessing) < 1){preprocessing <- list("RAW")}
      if (length(regions) < 1){regions <- list(c(9400,4250))}
    
    ind <- expand.grid(rep(list(c(1:length(regions))),length(regions)))
    ind <- unique(apply(X=ind,1,FUN=function(x){unique(sort(x))}))
    
    regions <- lapply(X=ind,FUN=function(x){regions[x]})
        
    combinations <- expand.grid(regions,preprocessing)
    colnames(combinations) <- c("region","preprocess")
  
  # TRAINING_SET
    
    if (is.null(training_set)){
      
      warning("No training set specified, optimizing with all calibration data.", call. = FALSE)
      
    }else{
      
      #component <- component[training_set]
      #spectra <- spectra[training_set,]
      
    }
    
  # LOOP THROUGH AND BUILD A CALIBRATION MODEL FOR ALL COMBINATIONS
  
    if (parallel == TRUE){
        nCores <- detectCores() - 1 # leave a core for user
        nCores <- ifelse(nCores<1, yes = 1, no = nCores) # make sure at least 1 core is used
        pls.options(parallel = makeCluster(nCores, type = "PSOCK"))
      }

    #spectra <- spectra[rev(order(training_set)),]
    #training_set <- training_set[rev(order(training_set))]
    #component <- component[rev(order(training_set))]
  
    for(i in 1:nrow(combinations)){
      
      r_selection <- matrix(unlist(combinations[i,"region"]),ncol=2,byrow=T) # this must cause a problem when it comes to specifying them yourself. check
      p_selection <- unlist(combinations[i,"preprocess"])
  
      # PREPROCESSING
      
      cur_spec <- spectra
      
      ############################################################################### FEB5
      
      if (is.null(training_set)){
      
        cur_spec_train <- cur_spec

      }else{
      
      cur_spec_train <- cur_spec[training_set,] # orig
      cur_spec_test <- cur_spec[!(training_set),] # orig
      
      }
      


      ############################################################################### FEB5      
      
      
      cur_spec_train <- subsetSpectra(spec=cur_spec_train,ranges=r_selection)
      if (!is.null(training_set)){cur_spec_test <- subsetSpectra(spec=cur_spec_test,ranges=r_selection)} # added if statement on feb5
        
        #for(p in p_selection){
          
          #if(class(cur_spec_train)=="spectra.matrix"){cur_spec_train <- as.spectra.list(cur_spec_train)}
          cur_spec_train <- preprocess(spec=as.spectra.list(cur_spec_train),transformation=p_selection)

    if (!is.null(training_set)){            
      if("MSC_reference" %in% names(attributes(cur_spec_train))){
          cur_spec_test <- preprocess(spec=as.spectra.list(cur_spec_test),transformation=p_selection,MSC_reference = attr(cur_spec_train,"MSC_reference"))
      }else{
          cur_spec_test <- preprocess(spec=as.spectra.list(cur_spec_test),transformation=p_selection)
      }
          
        }
      
      
      if (!is.null(training_set)){cur_spec <- rbind(cur_spec_train,cur_spec_test)}else{cur_spec <- cur_spec_train} # if else on feb5
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

      
      # SUBSET REGIONS
      
      #cur_spec <- subsetSpectra(spec=cur_spec,ranges=r_selection) #Orig
      
        # TYPE OF VALIDATION TO OPTIMIZE ON (i.e., LOO or CV)
          if (is.null(training_set)){
      
            cur_mod <- calibrate(component,cur_spec, validation = "LOO", parallel = FALSE, max_comps = max_comps) # parallel must be false because the loop is run in a persistent cluster (i.e., we dont want clusters within clusters)
            
            out <- data.frame(RMSECV=cur_mod$RMSECV,
                  Rank=cur_mod$rank,
                  Regions=paste(apply(r_selection,1,FUN=function(x){paste(x[1],x[2],sep=" - ")}),collapse=", "),
                  Preprocessing=paste(p_selection,collapse=", "))
            
          }else{
            
            cur_mod <- calibrate(component, cur_spec, training_set = training_set, validation = "testset", parallel = FALSE, max_comps = max_comps) # parallel must be false because the loop is run in a persistent cluster (i.e., we dont want clusters within clusters)
            
            out <- data.frame(RMSEP=cur_mod$RMSEP,
                  Rank=cur_mod$rank,
                  Regions=paste(apply(r_selection,1,FUN=function(x){paste(x[1],x[2],sep=" - ")}),collapse=", "),
                  Preprocessing=paste(p_selection,collapse=", "))
            
          }
      
      print(out)
      optimization_results <- rbind(optimization_results,out)
        param_subsets[[nrow(optimization_results)]] <- r_selection
        param_preproc[[nrow(optimization_results)]] <- p_selection
  
    }
      
    if (parallel == TRUE){
        stopCluster(pls.options()$parallel)
        pls.options(parallel = NULL)
    }
  
    if (is.null(training_set)){
      
        optimization_results <- optimization_results[with(optimization_results,order(RMSECV)),]
      
    }else{
      
        optimization_results <- optimization_results[with(optimization_results,order(RMSEP)),]

    }
  
  optout <- list(optimization_results = optimization_results,
                 param_subsets=param_subsets[as.numeric(row.names(optimization_results))],
                 param_preproc=param_preproc[as.numeric(row.names(optimization_results))])
            
  class(optout) <- "PLSopt"
  
  return(optout)
  
}