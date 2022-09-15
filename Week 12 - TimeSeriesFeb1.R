
# ****************************************************************************************
# This is the source script for WEEK 4: Time Series Analysis                             *
# ****************************************************************************************

# Created by Ben and Sally Rangecroft, Jan 2018
# adapted and updated by Valerie Ouellet and Benedikt Heudorfer, Feb 2019

#*****************************************************************************************

# Prepare R for new session
rm(list=ls()) # clear global environment 
dev.off() # close current graphics devices

# Download data and script files from Canvas
# Create new folder for your working directory, e.g. called 'Timeseries'

# Then redirect your working directory
#setwd("C:/Users/bened/Dropbox/R_time_series_practical")
# setwd("C:/Users/heudorfb/Dropbox/R_time_series_practical")
# setwd("/Users/MissO/Dropbox/R_time_series_practical")
setwd("C:/Users/ouelletv/Dropbox/R_time_series_practical")

#Q1:Which command do you use to check if the directory is ok?

#*****************************************************************************************
# PART 1.1: Introducing dates and times in R  *    
#*****************************************************************************************

#First, load the CSV files containing a temperature time series into R.
ouelleTemp <- read.csv(file.choose(), header=TRUE) #load the file 'ouelle_temperature.csv'

#Q2. Can you read a file directly instead of opening the folder? What's the command?

#Let's have a look at these variables
View(ouelleTemp)

# Seems like the time column has a different format. We can check this by feeding the class() 
# function into the sapply() function.
sapply(ouelleTemp, class)

# As you can see, the first variable/column ('dateTime') is stored as a 'factor' class, whereas 
# the others are stored as 'numeric'. This tells you that R doesn't automatically understand 
# that the 'dateTime' contains a date/time series. You need to tell R about the nature of 
# the data to be able to run time series analyses.

#**********

# We need to convert 'dateTime' to a class that R understands as dates. The most basic 
# date format is simply called 'Date'. Let's try it out.
Temptime_Date <- as.Date(ouelleTemp$dateTime)

# No error, that's a good start. But checking the output, we can see a problem.
head(Temptime_Date)

# What went wrong? Let's investigate the as.Date() function.
?as.Date()

# Ah, sub-header 'Usage' on the help page tells us that as.Date() by default tries to read
# dates in the format 'year-month-day' or 'year/month/day'. However, our date is in the 
# format 'day/month/year hour:minute'. This needs to be specified as an argument to the 
# function. This has to be done with certain codes, for example:

# %d	Day of the month (01-31)
# %m	Month (01-12)
# %W	Week (00-53)
# %y	Year without century (00-99)
# %Y	Year with century (1969-2068)

# full list in the help page of strptime:
help(strptime)

# Ok, so given our date format, the command should look like this:
Temptime_Date <- as.Date(ouelleTemp$dateTime, format = "%d/%m/%Y %H:%M")
head(Temptime_Date)

# This looks better. The date is in the correct format now, however the hours and minutes 
# are missing. Spoiler: The format 'Date' only accounts for days. It does so by counting the 
# number of days since 1970-01-01. You can check this by defining '1970-01-01' as a Date, and 
# then stripping it off its date property.
exampledate <- as.Date("1970-01-01")
as.numeric(exampledate)

# 0 days passed since 01. Jan 1970. One year later:
exampledate <- as.Date("1971-01-01")
as.numeric(exampledate)

# How many days passed at the first instance of our time series?
exampledate <- Temptime_Date[1]
exampledate
as.numeric(exampledate)

# Likewise, you can convert every (integer) number to a Date:
as.Date(15503)

# Uh-oh! The error message indicates that 'origin' must be supplied. As we just learned, the 
# origin is 1970-01-01. So:
as.Date(15503, origin = "1970-01-01")

# And there we are again. Btw you can define any date as the origin. For example today's date.
as.Date(15503, origin = "2019-02-06")

# But this gives us only the Date of the day 15503 days from now. Pretty pointless. It is 
# thus recommend to always stick to the original origin (i.e. 1970-01-01). Why is it exactly
# this date? It's essentially arbitrary, but was once defined to be Unix's birthday. Kind of 
# like day zero for Unix-based operating systems. For more info, read this super-old article:
# https://www.wired.com/2001/09/unix-tick-tocks-to-a-billion/

#**********

# Back on track. We still need to solve the problem that as.Date() omits hours and minutes.
# There is 2 more date formats native to R. POSIXct and POSIXlt. Let's try POSIXct.
Temptime_ct <- as.POSIXct(ouelleTemp$dateTime, format = "%d/%m/%Y %H:%M")
head(Temptime_ct)

# This looks right, but actually one last thing: as we are in Canada, we need to specify 
# the right time zone:
Temptime_ct <- as.POSIXct(ouelleTemp$dateTime, format = "%d/%m/%Y %H:%M", tz="Canada/Eastern")
head(Temptime_ct)

# Specifying time zone is not important for our toy dataset of one river in one location, and 
# it won't be necessary if you handle data that was recorded in a local or regional domain. 
# However it is vital for e.g. continental or global scale analyses. 

# NB. Some time zone specifiers (eg. "Europe/London" or "Canada/Atlantic" will automaticallly 
# switch between summer and winter time, whereas if you just say 'GMT' or 'BST', R will stick 
# data in these timezones regardless of the actual time of year of the dates). To see a list 
# of the time zones currently specifiable in R, type:
OlsonNames()

# Btw, POSIXct works the same way as Date, only that it counts SECONDS since 1970-01-01 01:00. 
exampledate <- as.POSIXct("1970-01-01 01:00")
as.numeric(exampledate)
Temptime_ct[1]
as.numeric(Temptime_ct[1]) #seconds since 1970-01-01 01:00

#**********

# The third format, POSIXlt, has the same property. 
Temptime_lt <- as.POSIXlt(ouelleTemp$dateTime, format = "%d/%m/%Y %H:%M", tz="Canada/Eastern")
as.numeric(Temptime_lt[1])

# However, POSIXlt comes with additional properties. We can reveal them by typing:
unclass(Temptime_lt[1])
unlist(Temptime_lt[200]) #alternative

# With POSIXlt, you can select certain time features individually, e.g. subsetting months:
Temptime_lt$mon

# Minutes:
Temptime_lt$min

# Looks very regular. Is it really?
unique(Temptime_lt$min) #Yes

# You can find explanations to all the POSIXlt time features here:
help(DateTimeClasses)

# As you can see, efficient subsetting with POSIXlt can be very handy to explore your data,
# and also in later stages of more complex coding. But POSIXlt can be horribly slow for larger 
# computations. This is because R wraps it not as a single number representing a day or second,
# but as a list of lists with the individual time features. 

View(Temptime_lt)

# Using POSIXct will yield fastest computing times if your data has hours and minutes. If you 
# have only days and no sub-daily times, 'Date' is simpler to use, and equally fast. 
# However, POSIXlt offers you those unique subsetting possibilities. But stripping out 
# days/months/etc in this manner strips reverts the class to a non-POSIXlt. To conclude, every 
# date format native to R has its pros and cons, and none appears ideal. 

#**********

# The package 'lubridate' solves this dilemma. 
library("lubridate") #install.packages("lubridate") if you haven't installed it yet

# Lubridate has a very easy syntax for loading different formating of dates. In our case,
# it is simply:
Temptime_lubridate <- dmy_hm(ouelleTemp$dateTime)

# The simplified Syntax converts the date/time series to POSIXct for fast computing times.
class(Temptime_lubridate)

# But it is simultaneously possible to subset it just like a POSIXlt object.
month(Temptime_lubridate) 
minute(Temptime_lubridate)

# These lubridate functions can also be applied to other date formats.
month(Temptime_Date)

# In addition to all the POSIXlt properties, luibridate has even more functionality. 
# For example, lubridate also lets you 'round' down dates, so that you just get the days 
# (or months or hours) over which observations occurred. This is useful if you're only 
# interested in daily values:
wholeDays <- unique(floor_date(Temptime_lubridate, unit="days"))
wholeDays

# Of course, this can be done as well in base R, but you'll have to switch to Date format:
wholeDays_Date <- unique(as.Date(Temptime_lubridate))
wholeDays_Date
class(wholeDays_Date)

# For more details about the Lubridate functions, go here: 
# https://cran.r-project.org/web/packages/lubridate/lubridate.pdf



# ****************************************************************************************
# PART 1.2: Extracting basic statistics from time series                                 *
# ****************************************************************************************

# ***** The data *****

# For this part of the class, you will be working with water temperature and discharge data 
# taken from the Riviere Ouelle, a salmon river in Quebec, Canada during summer 2012.  
# The Ouelle is a very warm river which often gets almost too hot for salmon and other fish 
# to survive in. It's therefore very important (from a river management point of view) to 
# get a picture of the temperature regime of the river in terms of daily maximum temperatures, 
# temperatures above a given threshold, etc.  


# First, load the CSV files containing temperature and flow data into R.
rm(list = ls())
ouelleTemp <- read.csv(file.choose(), header=TRUE) #load the file 'ouelle_temperature.csv'
ouelleFlow <- read.csv(file.choose(), header=TRUE) #load the file 'ouelle_flow.csv'
a<-ouelleFlow$dischargeOuelle

#Let's have a look at these variables
View(ouelleTemp)
View(ouelleFlow)

#Q3: what command could you use to quickly look at the beginning and end of you file to do a quick check?

#Before doing anything with the dataset, you need to verify for missing values, e.g. NAs
is.na(ouelleTemp)
is.na(ouelleFlow)

#Sometimes missing values are identified par another code e.g. -999, 
#you can search for these specific values and replace them
ouelleFlow$dischargeOuelle[ouelleFlow$dischargeOuelle==-999] 
ouelleFlow$dischargeOuelle[ouelleFlow$dischargeOuelle==-999]<- NaN 

#Q3. How do you check if the values were replaced?

#Want to know how many missing values exist? The output of is.na() is binary (TRUE/FALSE). 
#It can thus be summed up to check how many missing values are there in total. 
sum(is.na(ouelleTemp))
sum(is.na(ouelleFlow))

#Checking how many rows are without NA
sum(complete.cases(ouelleTemp)) #complete case
sum(!complete.cases(ouelleTemp))#! not complete case, same as sum(is.na(ouelleTemp))

#Try a basic stat without removing NaN. This will not work so smoothly.
mean(ouelleFlow$dischargeOuelle)

#How do we fix this?
#1. Excluding Missing Values from Analyses
mean(ouelleFlow$dischargeOuelle, na.rm=TRUE)

#see also na.fail(), na.omit(), na.exclude(), na.pass() for alternative methods
#generic functions are useful for dealing with NAs 

#2. Dealing with missing values by replacing them
#a. moving average
library("imputeTS") 

#Q4. Do you get an error? Is the package installed? How do you install it if not?

ouelleFlow$dischargeOuelle <-imputeTS::na.ma(ouelleFlow$dischargeOuelle, k = 6, weighting = "simple")

#b.interpolation
library(zoo)
test2 <- na.approx(a) #can also define an ## use equidistant spacing

#c. using a spline
test3 <- na.spline(a)

#other ways to look into rollapply, mean(sort(x[(na - window):(na + window)])), etc.

#Q5. How do you know if it worked? Do you still have missing values?

#**********

# Continuing, you'll notice that the temperature data is sampled at different times. Also, you 
# have two temperature series, 'temp_main_stem', sampled in the main river channel, and 
# 'temp_tributary', sampled from a cool tributary. We will use the 'temp_main_stem' dataset 
# for now.

# Now, define the date format
ouelleTemp$dateTime <- dmy_hm(ouelleTemp$dateTime)
ouelleFlow$dateTime <- dmy_hm(ouelleFlow$dateTime)

# R understands now that our data is a time series. Let's start extracting meaningful statistics 
# from the series by calculating a daily mean temperature from the 30-minute observations of 
# river temperature. 

# We first do this using a 'for-loop' that counts through each whole day, and gets the mean 
# of all temperature obs on this day.

# With loops, you regularly need to predefine or initialize some variables.
startDate <- min(yday(ouelleTemp$dateTime)) #First day with observations
endDate <- max(yday(ouelleTemp$dateTime)) #Last day with observations
dailyMean <- numeric() #Initialize an empty container for the loop output
n <- 1 #Initializing the index that will increment by 1 with each iteration

#Now, let's create the loop itself:

for (i in startDate:endDate){	#Iterate by 1 through all days from 'startDate' to 'endDate'.
   rowIdx <- which(yday(ouelleTemp$dateTime) == i)	#Indices of rows with observations on day i
   dailyMean[n] <- mean(ouelleTemp$temp_main_stem[rowIdx])	#Append mean temperature for all rows
   #in 'rowIdx' to the nth row in 'dailyMean'.
   n<-n+1 #Add 1 to n, to increment the indexing variable
} #The loop will execute when you close the squiggly bracket.		
rm(startDate,endDate,i,n,rowIdx) #clear up temporary variables

#Now, we can see the daily mean temperature in the study period:
View(dailyMean)

# Loops are the basic programming tool and you can do virtually everything with them. However,
# they have two main disadvantages:
# a) The code gets quite complicated quite fast, e.g. if you're using several for-loops. 
# b) Simultaneously, it gets really slow. 

#********************

# As always, there are alternative methods in R. Vectorized methods such as 'tapply' are much 
# faster. 'Tapply' allows you to apply a function (ie. mean, max, min, etc) to groups within a 
# given variable, (in our case, days of observations). To do so, you need to feed 3 arguments 
# to tapply: the series of temperature values, the index, and the function:
dailyMean <- tapply(ouelleTemp$temp_main_stem, yday(ouelleTemp$dateTime), mean) 

# Changing the function argument to tapply gives you any other kind of statistic
dailyMin <- tapply(ouelleTemp$temp_main_stem, yday(ouelleTemp$dateTime), min) 
dailyMax <- tapply(ouelleTemp$temp_main_stem, yday(ouelleTemp$dateTime), max)
dailySD <- tapply(ouelleTemp$temp_main_stem, yday(ouelleTemp$dateTime), sd)

# You can also use 'tapply' to count the number of observations per day:
numObs <- tapply(ouelleTemp$temp_main_stem, yday(ouelleTemp$dateTime), length)

# Like this, we can see that the no. of observations is the same for all days. 
numObs

# Another way of calculating grouped statistics is 'aggregate':
aggregate(ouelleTemp$temp_main_stem, list(yday(ouelleTemp$dateTime)), mean)

# 'aggregate' is more powerful than 'tapply', because it allows you to aggregate data by 
# multiple groups. But it can get more complicated.  Also, note the slightly different 
# output (it also includes a time/grouping component in a seperate column)

# Now let's tidy up the daily metrics we just created into a new data frame:
dailyTemp <- data.frame(wholeDays = unique(floor_date(ouelleTemp$dateTime, unit="days")), 
                        dailyMean, dailyMax, dailyMin, dailySD)
View(dailyTemp)

#********************

# Let's do a simple plot of this data. First, we plot the 30-min data and set the x-axis to 
# display weekly tick labels and date stamps every week
plot(ouelleTemp$dateTime, ouelleTemp$temp_main_stem, type = "l",
     ylab = "Temperature (?C)", xlab = "Date", 
     xaxt = "n", col = "grey50")
axis.POSIXct(1, at = seq(min(ouelleTemp$dateTime), 
                         max(ouelleTemp$dateTime), 
                         by = "week"), 
             format = "%y/%m/%d")

# Then, we can also plot the daily means on top using a double-thickness red line
lines(dailyTemp$wholeDays, dailyTemp$dailyMean,
      col = "red", lwd = 2)

#**********

# Alternatively, we can draw this data as a ribbon plot showing the mean temperature and 
# the standard deviation:

#First, create the upper and lower boundaries for the ribbon plot
upperSD <- dailyTemp$dailyMean + dailyTemp$dailySD 
lowerSD <- dailyTemp$dailyMean - dailyTemp$dailySD

#Then create an empty plot
plot(ouelleTemp$dateTime, ouelleTemp$temp_main_stem, type="n", 
     ylab="Temperature (?C)", xlab="Date", xaxt="n") 

# and set up the x-axis ticks to appear weekly
axis.POSIXct(1, at=seq(min(ouelleTemp$dateTime), 
                       max(ouelleTemp$dateTime), 
                       by="week"), 
             format="%y/%m/%d") 

# Next, draw the standard deviation ribbon using the 'polygon' function
polygon(c(rev(dailyTemp$wholeDays), dailyTemp$wholeDays), 
        c(rev(lowerSD), upperSD), 
        col = "grey85", border = FALSE) 

# Finally, plot the daily means on top using a double-thickness line
lines(dailyTemp$wholeDays,dailyTemp$dailyMean,col="red",lwd=2)

rm(upperSD, lowerSD) #clear up temporary variables


#**********

# As you might have noticed, the water temperature changes significantly during the day.
# The standard deviation ribbon already gives an indication of the extent. But let's have 
# closer look on the nature of the daily fluctuations.

# To do so, we need to first average hourly temperatures over the whole study period.
dailyTempFluct <- aggregate(ouelleTemp$temp_main_stem, 
                by = list(hour(ouelleTemp$dateTime)), 
                mean)

#Q6. How would you look at other time step. e.g. montlhy fluctuation?

# Let's tidy the output a little bit 
names(dailyTempFluct) <- c("hour","meanHourlyTemp")
dailyTempFluct$hour <- dailyTempFluct$hour + 1 #to have a standard 24h format

# Because the absolute temperature can vary significantly, it is more meaningful to look at 
# standardized numbers. To standardize, subtract the overall mean from the hourly average. 
dailyTempFluct$meanHourlyTemp <- dailyTempFluct$meanHourlyTemp - mean(dailyTempFluct$meanHourlyTemp)

#Let's see what we produced there 
plot(dailyTempFluct, pch = 16, xaxt = "n", 
     xlab = "Hour of day", ylab = "Temperature change (?C)") 
axis(1, at = c(1,6,12,18,24)) 

# This is the typical fluctuation of temperatures over the course of the day in the river. 
# Obviously there is also a big spread around the mean value. We can plot this as well like 
# previously as a standard deviation ribbon.

#First, calculate the daily sd. 
TempFlucSD <- aggregate(ouelleTemp$temp_main_stem, 
                by = list(hour(ouelleTemp$dateTime)), 
                sd)

#Add it to theexisting data frame, and delete the temporary object
dailyTempFluct$meanHourlyTempSD <- TempFlucSD$x
rm(TempFlucSD)

#Now we can calculate the upper and lower bound of the standard deviation range
dailyTempFluct$meanHourlyTempSD_upper <- dailyTempFluct$meanHourlyTemp + dailyTempFluct$meanHourlyTempSD
dailyTempFluct$meanHourlyTempSD_lower <- dailyTempFluct$meanHourlyTemp - dailyTempFluct$meanHourlyTempSD

#And finally visualize it with the the previously used methods
plot(dailyTempFluct$hour, dailyTempFluct$meanHourlyTemp, 
     type = "n", ylim = range(dailyTempFluct[,c(2:5)]), xaxt = "n",
     xlab = "Hour of day", ylab = "Temperature change (?C)")
axis(1, at = c(1,6,12,18,24)) 
polygon(c(dailyTempFluct$hour,rev(dailyTempFluct$hour)),
        c(dailyTempFluct$meanHourlyTempSD_upper,rev(dailyTempFluct$meanHourlyTempSD_lower)),
        col = "grey85", border = FALSE)
points(dailyTempFluct$hour, dailyTempFluct$meanHourlyTemp, pch = 16)

# As expected, there is a higher range temperature during the day. This is straight forward,
# because temperatures of surface waters are largely driven by sunlight.

# The plot illustrates a special feature of time series: Frequently our observations
# are dependant on recurring cycles. In this case, it is a daily cycle. Another frequently
# occurring cycle is the annual seasonal cycle. But there are also intra-annual, decadal, 
# multi-decadal cycles etc. Recurring cycles imply predictability, and are therefore often
# the aim of time series analyses. 

# Cyclicity also implies time-dependence. This poses a challenge to researchers, as most statistical
# methods are based on the assumption of independent samples. Thus, a large range of methods
# are not applicable in time series analysis. 



# ****************************************************************************************
# PART 1.3: Getting more complex info from time series                                   *
# ****************************************************************************************

# We can also calculate more complex statistics from time series data.
# For example, research has shown that Atlantic salmon suffer heat stress when temperatures 
# go above a given threshold (generally 23 ?C). River managers might therefore want to know 
# how many days had temperatures >= than 23 ?C.

# To get this number, we first create a vector of all times where temperature >= 23 ?C
thresholdExceeded <- ouelleTemp$dateTime[ouelleTemp$temp_main_stem >= 23]	

#overwrite this with a vector of the unique whole days on which 23 ?C was reached
thresholdExceeded <- unique(floor_date(thresholdExceeded, unit = "days"))

#Finally, you can count the number of unique days on which 23 ?C was reached
length(thresholdExceeded)

# You should see that out of 78 days (from Jun to Aug), on 57 days the water was warmer 
# than 23 ?C. Not good for salmon!


#**********

# Research has also shown that juvenile Atlantic salmon move to seek out cool water when the 
# number of degree-hours above 22 ?C reaches 61.52.  We can compute this statistic by converting
# our temperature observations from 30-minutes to hourly data.

# To do this, we first need to aggregate to hourly mean temperature. Note that we need to 
# average over two groups here, thus using aggregate instead of tapply
hourlyMean <- aggregate(x = ouelleTemp$temp_main_stem, 
                        by = list(hour(ouelleTemp$dateTime), yday(ouelleTemp$dateTime)),
                        FUN = mean)
hourlyMean <- hourlyMean$x #we only need to preserve the hourlyMean column

# Next, create a new 'hourly' time vector and use it to create a new tidy data frame:
hourlyTime <- seq(min(ouelleTemp$dateTime), max(ouelleTemp$dateTime), "hours")
hourlyTemp <- data.frame(hourlyTime, hourlyMean)
rm(hourlyMean, hourlyTime) #clear up temporary variables

# Then, add a new column to hourlyTemp with the difference between hourly means and 22 ?C
hourlyTemp$thresholdTemp <- hourlyTemp$hourlyMean - 22

# And replace all observations lower than 22 ?C with zero
hourlyTemp$thresholdTemp[hourlyTemp$thresholdTemp <= 0] <- 0 

# Finally, we sum the no. of hours on which 22 was exceeded, giving us the degree-hours  
# for each day in the study period
dailyTemp$degreeHours22 <- tapply(hourlyTemp$thresholdTemp, yday(hourlyTemp$hourlyTime), sum)

#**********

#To end this exercise, let's plot the degree-hours, and colour the instances when Atlantic 
# salmon might have moved (ie. points where the line goes above 61.52 degree hours). 

#First, create an empty plot
plot(dailyTemp$wholeDays, dailyTemp$degreeHours22, type = "n", 
     ylab = "Degree Hours > 22 ?C", xlab = "Date", xaxt = "n")

# Set up weekly x-axis ticks 
axis.POSIXct(1, at = seq(min(hourlyTemp$hourlyTime), 
                         max(hourlyTemp$hourlyTime),
                         by = "week"), 
             format = "%y/%m/%d")

# Then, interpolate the dates and degree-hours at a 1 minute resolution using the 'approx' 
# function. This is needed for the 'polygon' function to accurately draw the shaded area:
tempForPlotting <- approx(dailyTemp$wholeDays, 
                          dailyTemp$degreeHours22, 
                          xout = seq(min(ouelleTemp$dateTime),
                                     max(ouelleTemp$dateTime),
                                     "min"))
tempForPlotting$y[tempForPlotting$y < 61.52] <- 61.52 #set all values < 61.52 to 61.52 

# Next, draw the red-shaded area with 'polygon'
polygon(c(rev(tempForPlotting$x), tempForPlotting$x), 
        c(rep(61.52, rep(length(tempForPlotting$y))), 
          tempForPlotting$y), 
        col = "red", border = FALSE) 

# Finally, plot the daily degree-hours > 22 ?C
lines(dailyTemp$wholeDays, dailyTemp$degreeHours22, lwd = 2)

rm(tempForPlotting) #clear up temporary variables

# Looking at the plot, we can see that there were three occasions on which temperatures were 
# likely warm enough to threaten salmon to move to cooler water areas.


# ****************************************************************************************
# PART 1.4: Resampling time series data                                                  *
# ****************************************************************************************

# One problem that often happens with time series data is that different instruments record 
# observations at different sampling rates. This is the case with the temperature and flow 
# data for the River Ouelle.  The flow observations are at 15 minute intervals and the 
# temperature is every half-hour. Also, temperature was recorded every 11 and 41 minutes past 
# the hour, and flow was recorded at 00, 15, 30 and 45 minutes past the hour.  

# So how do we generate temperature and flow data with the same frequency and timing?

# An easy way is to change the temporal resolution using interpolation. Interpolation allows 
# us to resample observations at a desired frequency or point in time.  
# https://simple.wikipedia.org/wiki/Interpolation

# One common interpolation method is the cubic spline. It fits a curve through a set of data 
# points and then lets you specify the locations along the curve for which you would like to 
# generate data.

# Here, we will use spline interpolation to interpolate our 30-minute temperature observation 
# to 15-minute intervals so we can directly compare it to our flow observations.

# First, let's define our spline function (essentially the equation for a curve plotted 
# through the temperature obs)
tempSpline <- splinefun(x = ouelleTemp$dateTime, 
                        y = ouelleTemp$temp_main_stem, 
                        method = "fmm")

# Then, evaluate tempSpline at the desired times (where we have flow observations)
tempInterp <- tempSpline(as.numeric(ouelleFlow$dateTime))

# Comparing the interpolated series ('tempInterp') to the original ('ouelleTemp$temp_main_stem'), 
# we see that 'tempInterp' has twice the number of observations:
length(tempInterp)
length(ouelleTemp$temp_main_stem) 

#Finally, we can create a new 15-minute data frame containing both temperature and flow
ouelleTempAndFlow <- data.frame(dateTime = ouelleFlow$dateTime, 
                                tempInterp, 
                                discharge = ouelleFlow$discharge) 
View(ouelleTempAndFlow) 

#**********

#Finally, lets plot the new dataset. We'll do a plot with two y-axes showing temperature and 
# discharge on the same plot.

#First, plot temperature and set axis ticks to weekly intervals
par(mar = c(4, 5, 4, 5)) #define axis margins (give the plot space on the left for a second axis)
plot(ouelleTempAndFlow$dateTime, ouelleTempAndFlow$tempInterp, type = "l",
     ylab = "Temperature (?C)", xlab = "Date", xaxt = "n", col = "red")
axis.POSIXct(1, at=seq(min(ouelleTempAndFlow$dateTime), 
                       max(ouelleTempAndFlow$dateTime),
                       by="week"), 
             format="%y/%m/%d")

#Then, plot the discharge on a secondary axis.  
par(new = T) #tell R to plot the next graphics on the same plot
plot(ouelleTempAndFlow$dateTime, ouelleTempAndFlow$discharge, type="l", 
     xlab="", ylab="", xaxt="n", yaxt="n")
#Draw secondary axis
axis(4)
mtext(side = 4, line = 3, 'Discharge (m3s-1)')

#Draw a legend
legend("topright", legend = c("Temperature", "Discharge"), col = c("red", "black"), lty = 1)

par(new = F) #tell R to stop plotting graphics on the same plot


#*****************************************************************************************
# Part 1.5: Trend analysis                                                               *
#*****************************************************************************************

# Now, we already found evidence for a problem with water temperatures in the ouelle river, 
# which is the candidate reason for the mass fish death that occurred in the river during
# the observed period. 

# Another interesting question is, whether this problem will increase in the future following
# an aggravating climate change. We can answer this by looking for long-term trends in water
# temperature. To do this in a statistically meaningful way, we need long time series. However, 
# there are no long time series of water temperature available for the ouelle river. In case of 
# lacking data, a frequently used strategy is to use proxy measurements that might yield indications
# on the development of the desired variable. Here we will use air temperature measurements 
# of a close-by climate station as a proxy. 

# Loading climate_station.csv and formatting the date column
dt <-  read.table(file.choose(), sep = ",", header = TRUE)
dt$LOCAL_DATE <- dmy_hm(dt$LOCAL_DATE)

# Calculate the mean annual air temperature for the complete time series
annualmean <- aggregate(dt$MEAN_TEMPERATURE, list(year(dt$LOCAL_DATE)), mean)
# Rename the columns for clarity
names(annualmean) <- c("year","mean")

# As you learned, always do some basic cross checking on new data. This one has two NAs.
sum(is.na(annualmean$mean))

# Let's get rid of them.
annualmean$mean <- approx(annualmean$year, annualmean$mean)$y
sum(is.na(annualmean$mean)) #it worked

# Now a first look at the annual mean air temperature
plot(annualmean$year, annualmean$mean, pch = 16, col = "darkgrey")

# There seems to be a long-term increase in air temperature since the 1970. Let's see if we 
# can display it more distinctly. We can try a moving average for this. This function calculates 
# the average in a time window of +/- n years around every given year.

# For easier handling, we will define a function called "ma" for a moving average in a 10-year 
# moving window. This function has two arguments:
# x: time series object (only values, not times)
# n: length of moving window (here: years)
ma <- function(x, n = 10){ 
      stats::filter(x, rep(1/n, n), sides=2) }

# Let's apply the moving average and have a look at it.
annualmean$ma <- ma(annualmean$mean)
lines(annualmean$year, annualmean$ma, col = "red")

# The moving average reproduces the general trend in a locally explicit way. However, the plot 
# reveals the crucial disadvantage of the moving average: Calculation breaks down in the first 
# and last 5 years, as half of the moving window is not available in these locations. 

# A better alternative is the so-called LOESS method. In the same moving window, instead of 
# the mean, it calculates a locally smoothed linear regressions. 
loessData <- loess(mean ~ year, annualmean)
plot(annualmean$year, annualmean$mean, pch = 16, col = "darkgrey")
lines(loessData$x, loessData$fitted, col = "red3", lwd = 2)

# We can clearly see now a kind of step pattern, with temperatures jumping up in the 1990s. 

# Finally, the overall trend can be made visible with the 'lm' function (linear model):
linModel <- lm(annualmean$mean ~ annualmean$year)
abline(linModel, lwd = 3, lty = 2)

# This model also quantifies the average increase in temperature per year. It can be called 
# from the model output
linModel #two values are given: The intercept and the slope of the linear trend
linModel$coefficients[2] #accessing the slope

# Mean air temperatures increase by about ~0.04 degrees per year, which is a total of ~1.9 degrees
# over the study period (48 years)! If warming continues, and if we assume air temperature and 
# water temperature to be related, the fish in ouelle river will have a bad time in the future...

# This last part is just a glimpse at future curriculum. In reality, a lot of things have
# to be considered for coherent trend analysis, e.g. statistical significance. However, this 
# is left out here. Regression analysis will be fully introduced in week 6.



# ************************************************************
# EXERCISE 1: comparing main stem and tributary temperatures *
#                                                            *
# 1. Calculate the daily mean, maximum and minimum           *
#    temperatures in of the 'temp_tributary' series          *
# 2. Plot the daily mean temperature difference between the  *
#    tributary and the main stem                             *
# 3. Calculate the number of days on which temperatures in   *
#    the tributary exceeds 18 ?C                             *                                          *
# 4. Also, plot the trend in mean annual maximum daily       *
#    temperatures (column 'MAX_TEMPERATURE' in               *
#    climate_station.csv) and calculate the annual increase. *
#    Compare.                                                * 
#                                                            *
# ************************************************************

