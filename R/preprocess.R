preprocess <- function(spec,transformation,MSC_reference=NULL){
  
  if(is.data.frame(spec)){spec <- list(spec)}
  if(class(spec) == "spectra.matrix"){spec <- as.spectra.list(spec)}
  
  ele1 <- spec[[1]]
  cnames <- ele1$wave_value 
  
    if("subset_string" %in% names(attributes(spec))){
      
      subset_string <- attr(spec,"subset_string")
      subset_unique <- na.omit(unique(subset_string))
      
    }
  
  # BEGIN TRANSFORMATIONS # needs to be updated to allow different orders of operations
  
  processed <- spec
  processed <- lapply(X=processed,FUN=function(x){x$measurement})
  
    # RAW
      if("RAW" %in% transformation){
        #processed <- lapply(X=processed,FUN=function(x){x$measurement})
        #processed <- t(data.frame(processed))
      }
  
    # 1st DERIVATIVE
      if("D1f" %in% transformation){
        processed <- lapply(X=processed,FUN=function(x){
            
            if(exists("subset_string")){
              out_string <- rep(NA,length(x))
              for(i in subset_unique){
                cur_sub <- subset_string==i&!(is.na(subset_string))
                out_string[cur_sub] <- rev(locpoly(x=cnames[cur_sub],y=x[cur_sub],drv=1,bandwidth=17,gridsize=length(x[cur_sub]))[[2]])              
              }
              out_string
            }else{
              rev(locpoly(x=cnames,y=x,drv=1,bandwidth=17,gridsize=length(x))[[2]])              
            }                    
          })
        #processed <- t(data.frame(processed))
      }
  
    # 2nd DERIVATIVE
      if("D2f" %in% transformation){
        processed <- lapply(X=processed,FUN=function(x){
            
            if(exists("subset_string")){
              out_string <- rep(NA,length(x))
              for(i in subset_unique){
                cur_sub <- subset_string==i&!(is.na(subset_string))
                out_string[cur_sub] <- rev(locpoly(x=cnames[cur_sub],y=x[cur_sub],drv=2,bandwidth=17,gridsize=length(x[cur_sub]))[[2]])              
              }
              out_string
            }else{
              rev(locpoly(x=cnames,y=x,drv=2,bandwidth=17,gridsize=length(x))[[2]])              
            }                    
          })        
        #processed <- t(data.frame(processed))
      }  
  
    # CONSTANT OFFSET ELIMINATION
      if("COE" %in% transformation){
        processed <- lapply(X=processed,FUN=function(x){x-min(x, na.rm = T)})
        #processed <- t(data.frame(processed))
      }
  
#    # STRAIGHT LINE SUBTRACTION # ORIGINAL
#      if("SLS" %in% transformation){
#        processed <- lapply(X=processed,FUN=function(x){(x-predict(lm(x~cnames),newdata = data.frame(x,cnames)))})
#        #processed <- t(data.frame(processed))
#      }
  
    # STRAIGHT LINE SUBTRACTION
      if("SLS" %in% transformation){
        processed <- lapply(X=processed,FUN=function(x){
            
            if(exists("subset_string")){
              out_string <- rep(NA,length(x))
              for(i in subset_unique){
                cur_sub <- subset_string==i&!(is.na(subset_string))
                out_string[cur_sub] <- (x[cur_sub]-predict(lm(x[cur_sub]~cnames[cur_sub]),newdata = data.frame(x[cur_sub],cnames[cur_sub])))
              }
              out_string
            }else{
              (x-predict(lm(x~cnames),newdata = data.frame(x,cnames)))             
            }                    
          })          
        #processed <- t(data.frame(processed))
      }  
  
    # VECTOR NORMALIZATION
      if("SNV" %in% transformation){
        centered <- lapply(X=processed,FUN=function(x){x-mean(x, na.rm = T)})
        processed <- lapply(X=centered,FUN=function(x){x/sqrt(sum(x^2, na.rm = T))})
        #processed <- t(data.frame(processed))
      }  
  
    # MIN-MAX NORMALIZATION 
      if("MMN" %in% transformation){
        offset <- lapply(X=processed,FUN=function(x){x-min(x, na.rm = T)})
        processed <- lapply(X=offset,FUN=function(x){x*(2/max(x, na.rm = T))})
        #processed <- t(data.frame(processed))
      }

#    # MULTIPLICATIVE SCATTERING CORRECTION 
#      if("MSC" %in% transformation){
#        raw <- lapply(X=processed,FUN=function(x){x})
#        mean_spec <- apply(data.frame(t(data.frame(raw))),2,FUN=function(x){mean(x, na.rm = T)})
#        processed <- lapply(X=raw,FUN=function(x){(x-coef(lm(x~mean_spec))[1])/coef(lm(x~mean_spec))[2]})
#        #processed <- t(data.frame(processed))
#      }

    # MULTIPLICATIVE SCATTERING CORRECTION 
      if("MSC" %in% transformation){
        raw <- lapply(X=processed,FUN=function(x){x})

          if(is.null(MSC_reference)){

            if(exists("subset_string")){
              out_string <- matrix(NA,ncol = length(raw[[1]]),nrow = length(raw))
              colnames(out_string) <- cnames
              row.names(out_string) <- names(raw)
              all_means <- rep(NA,length(raw[[1]]))
              
              for(i in subset_unique){
                cur_sub <- subset_string==i&!(is.na(subset_string))
                    mean_spec <- apply(data.frame(t(data.frame(raw)))[,cur_sub],2,FUN=function(x){mean(x, na.rm = T)})
                out_string[,cur_sub] <- matrix(unlist(lapply(X=raw,FUN=function(x){(x[cur_sub]-coef(lm(x[cur_sub]~mean_spec))[1])/coef(lm(x[cur_sub]~mean_spec))[2]})),nrow = length(raw),ncol = length(mean_spec),byrow = T)
                all_means[cur_sub] <- mean_spec
              }
              processed <- lapply(as.spectra.list(as.spectra.matrix(out_string)),function(x){x$measurement})
              MSC_reference <- all_means
            }else{
              mean_spec <- apply(data.frame(t(data.frame(raw))),2,FUN=function(x){mean(x, na.rm = T)})
              processed <- lapply(X=raw,FUN=function(x){(x-coef(lm(x~mean_spec))[1])/coef(lm(x~mean_spec))[2]})
              MSC_reference <- mean_spec
            }
            
          }else{
            
            if(exists("subset_string")){
              out_string <- matrix(NA,ncol = length(raw[[1]]),nrow = length(raw))
              colnames(out_string) <- cnames
              row.names(out_string) <- names(raw)
              
              for(i in subset_unique){
                cur_sub <- subset_string==i&!(is.na(subset_string))
                    mean_spec <- MSC_reference[cur_sub]
                out_string[,cur_sub] <- matrix(unlist(lapply(X=raw,FUN=function(x){(x[cur_sub]-coef(lm(x[cur_sub]~mean_spec))[1])/coef(lm(x[cur_sub]~mean_spec))[2]})),nrow = length(raw),ncol = length(mean_spec),byrow = T)
              }
              processed <- lapply(as.spectra.list(as.spectra.matrix(out_string)),function(x){x$measurement})
            }else{
              mean_spec <- MSC_reference
              processed <- lapply(X=raw,FUN=function(x){(x-coef(lm(x~mean_spec))[1])/coef(lm(x~mean_spec))[2]}) 
            }            
            
          }
        
        #attr(processed,"MSC_reference") <- MSC_reference
        
        #mean_spec <- apply(data.frame(t(data.frame(raw))),2,FUN=function(x){mean(x, na.rm = T)})
        #processed <- lapply(X=raw,FUN=function(x){(x-coef(lm(x~mean_spec))[1])/coef(lm(x~mean_spec))[2]})                                

        #processed <- t(data.frame(processed))
      }

###
  
    #if("MSC_reference" %in% names(attributes(processed))){

      #MSC_reference <- attr(processed, "MSC_reference")
      
    #}

  processed <- t(data.frame(processed))
  
  colnames(processed) <- cnames
  class(processed) <- "spectra.matrix"
  processed <- as.matrix(processed)

    if("subset_string" %in% names(attributes(spec))){

      attr(processed, "subset_string") <- subset_string
      
    }
    if(!is.null(MSC_reference)){
      
      attr(processed, "MSC_reference") <- MSC_reference
      
    }

    if("wave_unit" %in% names(attributes(ele1))){

      attr(processed, "wave_unit") <- attr(ele1, "wave_unit")
      
    }

    if("measurement_unit" %in% names(attributes(ele1))){

      attr(processed, "measurement_unit") <- attr(ele1, "measurement_unit")
      
    }

  return(processed)
}