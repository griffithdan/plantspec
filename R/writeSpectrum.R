writeSpectrum <- function(x, filename){
  spec <- x[c("wave_value","measurement")]
  write.table(x = spec, file = filename,row.names = F,col.names = F, eol = "\n", sep="\t", quote=F)  
}