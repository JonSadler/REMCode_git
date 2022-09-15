# Script to calculate a threshold, do drought analysis and plot droughts 
# based on: Van Loon (2015). Hydrological drought explained. WIRES Water, 2(4), 359-392.
#
#
# Anne van Loon
# University of Birmingham
#
# v0: 23.05.2016
# v1: 24.07.2016 (incl. max intensity for state variables)
#
#####################################################################

rm(list=ls())
#setwd("...")

### read data
datafile  = read.table("....txt",header=TRUE,sep='\t')  # read in data
dates		  = as.Date(datafile$...,format="%Y-%m-%d")	    # get dates from data
var       = ...                                         # variable to use in analysis

# EXAMPLE
datafile 	= read.table("Guadiana_60-80.txt", header=TRUE, sep = "\t") # you can use the same timeseries from which you will calculate the drought characteristics or a different one
dates		  = as.Date(datafile$date.yyyymmdd,format="%Y-%m-%d")	
var       = filter(datafile$Psim, rep(1/10,10),sides=2)   # for precipitation (or other variables) a moving average can be applied, in this case 10days   
var       = datafile$Qobs  # discharge, you can use more than one variable
var       = datafile$SLZ  # groundwater, note that for state variables maximum intensty is used instead of deficit volume

# EXAMPLE data Sally
#datafile    = read.table("Q3806001_1965-2013.csv",header=TRUE,sep=' ')
#dates		    = as.Date(datafile$Index,format="%Y-%m-%d")	
#var         = datafile$discharge

Jdays = strptime(dates,format="%Y-%m-%d")$yday+1  	# extract Julian days from dates

###############################################################################################################

#### THRESHOLD

### load threshold functions
source("Threshold_functions.R")

### calculate fixed and/or varibale threshold
# FixThres: fixed threshold
# VarThres: variable threshold based on 30d moving window
# "data" = data variable, "percentile" = percentile of FDC (exceedance), "dates" = date variable, "Jdays" = Julian days
threshold=FixThres(var,...,dates)
threshold=VarThres(var,...,dates,Jdays)

# EXAMPLE
threshold=FixThres(var,0.8,dates)   # 0.8 = 80th percentile, 0.95 = 95th percentile
threshold=VarThres(var,0.8,dates,Jdays)

### write file
write.table(threshold,"....txt")

# EXAMPLE
write.table(threshold,"fixed_threshold.txt")
write.table(threshold,"variable_threshold_P.txt")
write.table(threshold,"variable_threshold_Q.txt")

###############################################################################################################

#### DROUGHT ANALYSIS

### load drought functions
source("Drought_functions.R")

## fixed threshold: use same value for entire timeseries
thres = rep(threshold[[1]], length(var))

## variable threshold: use 365 (/366) values to fill timeseries
thres = array()
for(i in 1:length(Jdays)){
  thres[i] = threshold[Jdays[i],1]
}

### calculate drought characteristics
## "data" = data variable, "threshold" = threshold variable, "dates" = date variable
droughts=Droughtchar(var,thres,dates)

## output for each drought event: "st" = start, "end" = end, "dur" = duration, "def" = deficit volume (use  
##      for fluxes, like P & Q), "maxint" = maximum intensity (use for state variables, like SM & GW)
names(droughts)[4]="def"
names(droughts)[5]="maxint"
droughts$dur <- as.numeric(as.character(droughts$dur))
droughts$def <- as.numeric(as.character(droughts$def))
droughts$maxint <- as.numeric(as.character(droughts$maxint))

### write file
write.table(droughts,"....txt")

# EXAMPLE
write.table(droughts,"droughts_fixed.txt")
write.table(droughts,"droughts_variable_P.txt")
write.table(droughts,"droughts_variable_Q.txt")

###############################################################################################################

#### PLOT

### plot timeseries
# change plot settings if needed
pdf("plot_Q.pdf", width=12, height=8)	# save as PDF
plot(dates,var,type="l")
lines(dates,thres, col="red")
graphics.off()

### plot drought charactersitics, can be used for propagation if multiple variables are plotted
# change plot settings if needed
deficit = as.numeric(droughts$def)
duration = as.numeric(droughts$dur)
regression = lm(formula = deficit~duration, x = TRUE, y = TRUE)
coeff = coefficients(regression)

pdf("plot_droughtchar_Q.pdf", width=8, height=8)	# save as PDF
plot(duration,deficit)
abline(regression, col="blue") # regression line (y~x)
mtext(signif(coeff[2], digits=4),side = 3, col="blue")
graphics.off()

### plot droughts, can be used for propagation if multiple variables are plotted
# change plot settings if needed
t.polygon = dates
thres.polygon = thres
var.polygon = var
#t.polygon = dates[10:(length(dates)-10)]     # if moving average is used > remove NAs
#thres.polygon = thres[10:(length(thres)-10)]
#var.polygon = var[10:(length(thres)-10)]
pdf("plot_droughts_Q.pdf", width=12, height=8)	# save as PDF
plot(dates,var,type="l",ylim=c(0,3*max(thres)))
polygon(c(t.polygon,t.polygon[length(t.polygon):1]),c(var.polygon,pmax(var.polygon,thres.polygon)[length(var.polygon):1]), col="red", border=0)
lines(dates,var)
lines(dates,thres, lty=2, lwd=2)
graphics.off()

###############################################################################################################

#### POOLING

### load pooling functions
source("Pooling_functions.R")

### pooling droughts
## Pooling: "st" = drought starting date variable, "dur" = drought duration variable,   
##          "def" = drought deficit variable, "maxint" = maximum intensity variable, "IT" = interevent time period 
droughts=Pooling(...,...,...,...,...,...)

# EXAMPLE: interevent time period = 10 days
droughts=Pooling(droughts$st,droughts$dur,droughts$def,droughts$maxint,10)

## write file
write.table(droughts,"....txt")
write.table(droughts,"droughts_pooled_P.txt")
write.table(droughts,"droughts_pooled_Q.txt")
write.table(droughts,"droughts_pooled_GW.txt")

### removing minor droughts 
## subset of droughts with duration larger than x days
## Minor: "file" = data variable, "dur" = drought duration variable, "MIN" = minimum drought duration
droughts=Minor(...,...,...)

# EXAMPLE: min drought duration = 15
droughts=Minor(droughts,droughts$dur,15) 

## write file
write.table(droughts,"....txt")
write.table(droughts,"droughts_minor_P.txt")
write.table(droughts,"droughts_minor_Q.txt")

###############################################################################################################

#### CHARACTERISTICS

### load pooling functions
source("Characteristics_functions.R")

## calculate average & max drought characteristics
avg=Average(...,...,...)
avg=Average(droughts$dur,droughts$def,droughts$maxint)

max=Maximum(...,...,...)
max=Maximum(droughts$dur,droughts$def,droughts$maxint)

## write file
write.table(droughts,"....txt")
write.table(data.frame(avg,max),"droughts_char_P.txt")
write.table(data.frame(avg,max),"droughts_char_Q.txt")

###############################################################################################################
