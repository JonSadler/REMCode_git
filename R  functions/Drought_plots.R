# Script to plot droughts & drought charactersitics with threshold
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
datafile 	  = read.table("Guadiana_60-00.txt", header=TRUE, sep = "\t") 
dates		    = as.Date(datafile$date.yyyymmdd,format="%Y-%m-%d")	
var         = filter(datafile$Psim, rep(1/10,10),sides=2)   # for precipitation (or other variables) a moving average can be applied, in this case 10days   
var       = datafile$Qobs  # discharge, you can use more than one variable
var       = datafile$SLZ  # groundwater, note that for state variables maximum intensty is used instead of deficit volume

Jdays = strptime(dates,format="%Y-%m-%d")$yday+1  	# Julian days

### read threshold file
thresholdfile=read.table("....txt",header=TRUE,sep='\t')

# EXAMPLE
thresholdfile=read.table("fixed_threshold.txt",header=TRUE,sep=' ')
thresholdfile=read.table("variable_threshold_P.txt",header=TRUE,sep=' ')
thresholdfile=read.table("variable_threshold_Q.txt",header=TRUE,sep=' ')

### read drought file
droughtfile=read.table("....txt",header=TRUE,sep='\t')

# EXAMPLE
droughtfile=read.table("droughts_fixed.txt",header=TRUE,sep=' ')
droughtfile=read.table("droughts_variable_P.txt",header=TRUE,sep=' ')
droughtfile=read.table("droughts_variable_Q.txt",header=TRUE,sep=' ')

## fixed threshold: use same value for entire timeseries
thres = rep(thresholdfile[[1]], length(var))

## variable threshold: use 365 (/366) values to fill timeseries
thres = array()
for(i in 1:length(Jdays)){
  thres[i] = thresholdfile[Jdays[i],1]
}

### plot timeseries
# change plot settings if needed
pdf("plot_Q.pdf", width=12, height=8)	# save as PDF
plot(dates,var,type="l")
lines(dates,thres, col="red")
graphics.off()

### plot drought charactersitics, can be used for propagation if multiple variables are plotted
# change plot settings if needed
regression = lm(formula = droughtfile$def~droughtfile$dur, x = TRUE, y = TRUE)
coeff = coefficients(regression)

pdf("plot_droughtchar_Q.pdf", width=8, height=8)	# save as PDF
plot(droughtfile$dur,droughtfile$def)
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


