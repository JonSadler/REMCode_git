## Functions to compute drought threshold
## VarThres: variable monthly threshold 
## "data" = data variable, "percentile" = percentile of FDC (exceedance), "dates" = date variable



VarThres <- function(data, percentile, dates){  #no Jdays anymore
  perc = 1-percentile
  vthreshold = array()
  datafilter = filter(data, rep(1/1,1),sides=1) #Still included because of the last line in the for-loop
  for(d in 1:9){                                
    month = paste("0",d,sep="")
    month1 = (mm == month)                      #mmis defined in the analysis script
    
    sel = which(month1)                         
    vthreshold[d] = quantile(datafilter[sel], probs=(perc), na.rm=T)
  }
  for(d in 10:12){
    month = d
    month1 = (mm == month)
    
    sel = which(month1)
    vthreshold[d] = quantile(datafilter[sel], probs=(perc), na.rm=T)
  }
  return(as.data.frame(vthreshold))
}

