# Script to calculate drought charactersitics from threshold and 
# write to file 
#
#
# Anne van Loon
# University of Birmingham
#
# 10.05.2016
#
#####################################################################

rm(list=ls())
#setwd("...")

### read data
datafile    = read.table("....txt",header=TRUE,sep='\t')
dates		    = as.Date(datafile$...,format="%Y-%m-%d")	
var         = ... #variable to use in analysis

# EXAMPLE
datafile 	  = read.table("Guadiana_60-80.txt", header=TRUE, sep = "\t") # you can use the same timeseries from which you calculated the threshold
datafile 	  = read.table("Guadiana_60-00.txt", header=TRUE, sep = "\t") # or you can use another one
dates		    = as.Date(datafile$date.yyyymmdd,format="%Y-%m-%d")	
var         = filter(datafile$Psim, rep(1/10,10),sides=2)   # for precipitation (or other variables) a moving average can be applied, in this case 10days   
var       = datafile$Qobs  # discharge, you can use more than one variable
var       = datafile$SLZ  # groundwater, note that for state variables maximum intensty is used instead of deficit volume

Jdays = strptime(dates,format="%Y-%m-%d")$yday+1  	# extract Julian days from dates

### read threshold file
thresholdfile=read.table("....txt",header=TRUE,sep='\t')

# EXAMPLE
thresholdfile=read.table("fixed_threshold.txt",header=TRUE,sep=' ')
thresholdfile=read.table("variable_threshold_P.txt",header=TRUE,sep=' ')
thresholdfile=read.table("variable_threshold_Q.txt",header=TRUE,sep=' ')

### load drought functions
source("Drought_functions.R")

## fixed threshold: use same value for entire timeseries
thres = rep(thresholdfile[[1]], length(var))

## variable threshold: use 365 (/366) values to fill timeseries
thres = array()
for(i in 1:length(Jdays)){
  thres[i] = thresholdfile[Jdays[i],1]
}

### calculate drought characteristics
## "data" = data variable, "threshold" = threshold variable, "dates" = date variable
droughts=Droughtchar(var,thres,dates)

## output for each drought event: "st" = start, "end" = end, "dur" = duration, "def" = deficit volume (use  
##      for fluxes, like P & Q), "maxint" = maximum intensity (use for state variables, like SM & GW)
names(droughts)[4]="def"
names(droughts)[5]="maxint"

### write file
write.table(droughts,"....txt")

# EXAMPLE
write.table(droughts,"droughts_fixed.txt")
write.table(droughts,"droughts_variable_P.txt")
write.table(droughts,"droughts_variable_Q.txt")
