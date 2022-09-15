## Functions to compute drought threshold
## FixThres: fixed threshold
## VarThres: variable threshold based on 30d moving window
## "data" = data variable, "percentile" = percentile of FDC (exceedance), "dates" = date variable, "Jdays" = Julian days

FixThres <- function(data, percentile, dates){
  perc = 1-percentile
  fthreshold = array()
  fthreshold = quantile(data, probs=(perc), na.rm=T)
  return(as.data.frame(fthreshold))
}

VarThres <- function(data, percentile, dates, Jdays){
  perc = 1-percentile
  vthreshold = array()
  datafilter = filter(data, rep(1/30,30),sides=2)
  for(d in 1:366){
    sel <- which(Jdays == d)
    vthreshold[d] = quantile(datafilter[sel], probs=(perc), na.rm=T)
  }
  return(as.data.frame(vthreshold))
}
