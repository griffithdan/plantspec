#' Function to preprocess on spectra.
#' 
#' This function accepts spectra, performs the desired transformations, and
#' returns the results. The full list of available transformations is specified
#' below under the parameter "transformation." 
#' 
#' 
#' @param spec An object of class \code{spectra.list} or \code{spectra.matrix}.
#' @param transformation The desired transformation. Selected from:\cr "RAW" -
#' The raw spectrum, as supplied, is returned.\cr "D1f" - The first
#' derivative.\cr "D2f" - The second derivative.\cr "COE" - Constant Offset
#' Elimination.\cr "SLS" - Straight Line Subtraction.\cr "SNV" - Vector
#' Normalization.\cr "MMN" - Min/Max Normalization.\cr "MSC" - Multiplicative
#' Scattering Correction (Isaksson and Næs, 1988).
#' @param MSC_reference A reference used in MSC prepreocessing. If NULL, the
#' mean spectra of the provided spectra is used.
#' @return Returns an object of class \code{spectra.matrix}. If MSC was
#' performed, the mean/reference spectra is returned as an attribute called
#' "MSC_reference."
#' @references Tomas Isaksson and Tormod Næs, "The Effect of Multiplicative Scatter Correction (MSC) and Linearity Improvement in NIR Spectroscopy," Appl. Spectrosc. 42, 1273-1284 (1988) 
#' @author Daniel M Griffith
#' @keywords manipulation
#' @examples
#' 
#' 
#' #data(shootout)
#' #processed_data <- preprocess(spec = shootout_scans, transformation = "COE")
#' #par(mfrow=c(1,2))
#' #plot(shootout_scans)
#' #plot(processed_data)
#' 
#' 
#' @export preprocess
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
  
  processed[is.na(processed)] <- 0

  return(processed)
}
