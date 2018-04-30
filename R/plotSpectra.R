plotSpectra <- function(x, col = "black",type = NULL, xlim = NULL, ylim = NULL, add = FALSE, base_plot = FALSE, ...){

  if(base_plot == TRUE){  
    if(class(x) == "spectra.matrix"){
      x <- as.spectra.list(x)
    }

    if(!(exists("add"))|if(exists("add")){ifelse(add==FALSE,TRUE,FALSE)}){
          if(exists("add")){rm(add)}
  
      if(is.null(ylim)){
        ymax <- max(unlist(lapply(X = x,FUN = function(s){max(s$measurement,na.rm = T)})),na.rm = T)
        ymin <- min(unlist(lapply(X = x,FUN = function(s){min(s$measurement,na.rm = T)})),na.rm = T) 
        ylims <- c(ymin,ymax)
      }else{ylims = ylim; ylim = NULL}
      if(is.null(xlim)){
        xmax <- max(unlist(lapply(X = x,FUN = function(s){max(s$wave_value,na.rm = T)})),na.rm = T)
        xmin <- min(unlist(lapply(X = x,FUN = function(s){min(s$wave_value,na.rm = T)})),na.rm = T) 
        xlims <- rev(c(xmin,xmax))
      }else{xlims = xlim; xlim = NULL}
      if(is.null(type)){
      linetype = "l"
      }else{linetype = type; type = NULL}
    
      yvals <- x[[1]]$measurement
      xvals <- x[[1]]$wave_value
      plot(yvals ~ xvals,ylim=ylims,xlim=xlims,type = linetype,
          ylab = Hmisc::capitalize(attr(x[[1]],"measurement_unit")), 
          xlab = Hmisc::capitalize(attr(x[[1]],"wave_unit")),
          col = col, ...)# reg mode
      #plot(yvals ~ xvals,type = "l",ylab = "Absorbance", xlab = "Wavenumber",col = col,main=names(x)[1]) # checkmode
      
      #readline()
      if(length(x)>1){
        for (i in 2:length(x)){
          
          yvals <- x[[i]]$measurement
          xvals <- x[[i]]$wave_value
          points(yvals ~ xvals,type = linetype, col = col, ...) # reg mode
          
          # check mode
            #plot(yvals ~ xvals,type = "l",ylab = "Absorbance", xlab = "Wavenumber",col = col,main=names(x)[i]) # a whole section of scans is backwards????
      
            #readline()
        }}
    }else{if(add==T){
      
      rm(add)
      
      if(is.null(type)){
      linetype = "l"
      }else{linetype = type; type = NULL}
    
      yvals <- x[[1]]$measurement
      xvals <- x[[1]]$wave_value
      points(yvals ~ xvals,type = linetype,
          col = col, ...)# reg mode
      #plot(yvals ~ xvals,type = "l",ylab = "Absorbance", xlab = "Wavenumber",col = col,main=names(x)[1]) # checkmode
      
      #readline()
      if(length(x)>1){
        for (i in 2:length(x)){
          
          yvals <- x[[i]]$measurement
          xvals <- x[[i]]$wave_value
          points(yvals ~ xvals,type = linetype, col = col, ...) # reg mode
          
          # check mode
            #plot(yvals ~ xvals,type = "l",ylab = "Absorbance", xlab = "Wavenumber",col = col,main=names(x)[i]) # a whole section of scans is backwards????
      
            #readline()
        }}
      
    }}
    
  } else {
    
    if(class(x) == "spectra.list"){
          x <- as.spectra.matrix(x)
    }
    
    xlab <- Hmisc::capitalize(attr(x,"wave_unit"))
    ylab <- Hmisc::capitalize(attr(x,"measurement_unit"))
    
    class(x) <- "matrix"

      bds <- floor(seq(from = 1, to = ncol(x), length.out = 200))
      bds <- as.numeric(colnames(x))[bds]
 
   x <- melt(data = x)
    
   colnames(x) <- c("Scan","Band","Value")
   
   # rbokeh version
   # x.sub <- x[x$Band %in% bds,]
   #  figure(ylab = "Absorbance", xlab = "Wavenumber", xgrid = FALSE, ygrid = FALSE) %>%
   #    ly_lines(Band, Value, color = Scan, data = x, legend = FALSE)  %>% # for user colors it has to be group and then the user colors as "color" of the same length
   #    ly_points(Band, Value, group = Scan, data = x.sub, legend = FALSE, hover = c(Scan), visible = FALSE) # for user colors it has to be group and then the user colors as "color" of the same length
    
   
a <- list(
  ticks = "outside",
  showline = TRUE,
  showticklabels = TRUE,
  showgrid = FALSE,
  mirror = TRUE
)  

xax <- a
yax <- a
xax$title <- xlab
yax$title <- ylab
   
p <- plot_ly(x, 
             x = ~Band, y = ~Value, color = ~Scan, 
             colors = rainbow(length(unique(x$Scan))),
             hoverinfo = 'text',
             text = ~paste('Scan: ', Scan,
                           '</br></br> Band: ', Band,
                           '</br> Value: ', Value)) %>%
    layout(xaxis = xax,
           yaxis = yax) %>%
  add_lines(line = list(width = 0.75))
p 
   
  }
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  