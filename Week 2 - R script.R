# *************************************************************************
# This is the source script for WEEK 2: Importing data and manipulating it*
# *************************************************************************
# Jon Sadler Jan 2014 updated Jan 2021 [updated for github Sept 2022]

#  **********************************
#  ****** FIRST IMPORTANT TASK ****** 
#  **********************************

#Check where R is looking at your working directory. If not set it.

getwd()

# *********************************************
# PART 1: Getting Data into R via file import *
# *********************************************
# 
# Data input from textfiles and excel spreadsheets and choose.file() 
# Make sure you have downloaded the datasets you need from the website! You need squid.txt, deer.txt, compensation.csv
# Make sure they are in your workspace!

# Using the read.table function for "tab delineated" data

Squid <- read.table(file = "squid.txt", header = TRUE)

# We strongly recommend that you use the names, str, head, tail and dim functions after import
# You need to be sure you've got the correct dataset!!

names(Squid)        # Lists the column (or variable names)
str(Squid)          # Indicates the structure of the data frame (i.e. variable types etc)
head(Squid)         # Lists the first 6 rows of data
tail(Squid)         # No prizes for guessing what that does....
dim(Squid)          # Lists the dimensions of the data frame (in this case 2644 cases or rows and 6 variables or columns)

# Importing an Excel (or any other spreadsheet) CVS file (comma-delineated file)
# Make sure your data are formatted correctly with variables in columns. Use sensible column names
# And don't use these characters are R doesn't like them: £, $, %, ^, ,, ., <, >, ?, \, |, [], {} etc
# Make sure missing values have a 'NA' and they are not left blank

Comp.dat <- read.csv(file = "compensation.csv", header = TRUE)  
names(Comp.dat)
str(Comp.dat)         # We'll come back to this one in part 3
head(Comp.dat)
Comp.dat               # Lists the whole file

# For the super lazy or very efficient you can seek a CSV data file using the following:
# Try it and import the data file called "ozone.csv" 

Ozone <-read.csv(file.choose(), header=TRUE)
names(Ozone)
str(Ozone)
head(Ozone)

# Beauty of this is that your data files don't have to sit in your working directory 
# If you use this command though remember to annotate into your R script where the data file is and its name
# You'll forget otherwise!!!


# ******************************************************
# PART 2: Transforming data - Summarising, aggregating *
#                             filtering, mutating      *
# ******************************************************
# The next two sections will introduce you to important functions:
# tapply(),  table(), in base R
# aggregate(), summarise(), mutate() (in dplyr)
# AND gather(), spread() and separate(), in tidyR
# You'll need a different dataset for this "Vegetation.csv
# This data file holds data on vegetation transects in a multi-year study in the US (further information in Zuur et al.)
# We'll import this one with the read.csv file.choose() function
# You'll need to install the following package (R will install their dependencies):
# Tidyverse (it has ggplot, dplyr, tidyr and broom - brilliant packages!)


# You'll almost certainly need to install these. We'll use code to do this:
install.packages("tidyverse") # This is Hadley Wickham's famous suite of programs, we'll come to them later...
library(tidyverse)

# as always we could have used on line: install.packages(c("dplyr", "tidyr")) to load the components not the full suite

# download datafile for work....
Veg <- read.csv(file.choose(), header=TRUE)
names(Veg)
str(Veg)

# The tapply function to create summaries of your variables (means, sd etc)
# Let's say you're interested in finding how the mean species richness (R in the data file) differs across transects:
unique(Veg$Transect)
# 8 transects - a lot of effort. Fortunately, one function will do that for you:

tapply(Veg$R, Veg$Transect, mean)       # here are your 8 means. 
# or alternative syntax: tapply(X = veg$R, INDEX = veg$Transect, FUN = mean)
# Used when you are looking for functions on subsets of a variable conditional on another variable generally a factor

# You can use any arithmetic function sd, median var (variance)
Me <- tapply(Veg$R, Veg$Transect, mean)    # Calculates the mean
Sd <- tapply(Veg$R, Veg$Transect, sd)      # Calculates the sd
Le <- tapply(Veg$R, Veg$Transect, length)  # Calculates the number of observations (length)
Descriptors <- cbind(Me, Sd, Le)
Descriptors

# we can use dplyr to do this much easier with less code
require(dplyr)
# Key functions in this library are:
# select() - allows you to select columns in a dataframe for further manipulation
# filter() - subsetting or removing observations based on some condition.
# group_by() - groups variables by columns
# summarise() - provides summary statistics for a group of observations (e.g. mean, SD etc)
# arrange() - orders dataframes by observations (it's a sorting function)
# join() - allows dataframes to be joined using unique attribute IDs
# mutate() - create new columns on the basis of calculations and transforms of other columns

# So let's repeat the above operation with dplyr. It can be done with one line of code.

Descriptors <- summarise(group_by(Veg, Transect), mean=mean(R), sd=sd(R), length=length(R))

# have a lookat the summary data. Big plus here is that the result is saved as a dataframe
Descriptors

# You can also use the piping operators (%>%) to build highly complex wrangles. Here's the one we've just done...
# We'll revisit it later with a different datasets.

Veg %>% group_by(Transect) %>% summarise(mean=mean(R), sd=sd(R), length=length(R))

# I like this code. It basically says. Take the veg dataframe, group observations by transect, then
# provide the mean, standard deviation and length of each transect. If you want to store it in an object then
# just add Descriptors <-  before the code. This will then give you a dataframe! Double bonus!!

# filtering allows you to subset data. You've already done this the hard way already last session.
# dplyr makes it easier and quicker.

filter(Veg, Transect == 1:2) # displays all the records in transects 1 and 2

# So you can see that by using combinations of these commands you can pull out, summarise and analyses subsets
# of data from large data files very quickly and effectively. For example, we can use the above code and then arrange the 
# the data by transect

VegT12 <- filter(Veg, Transect == 1:2) # displays all the records in transects 1 and 2

arrange(VegT12, BARESOIL)     # sorts your vegT12 in ascending order by the amount of baresoil in each quadrat.

# Transform using mutate using the sleep database on mammal sleep time
#load data (datafile is: MammalSleep,csv). See http://genomicsclass.github.io/book/pages/dplyr_tutorial.html for metadata
sleep <- read.csv(file.choose(), header=TRUE)
names(sleep)
str(sleep)

sleep1 <- sleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total, 
         bodywt_grams = bodywt * 1000) # adds a new column showing the proportion of sleep for each species.
sleep1     #Look at your new dataframe with the additional 'new' column.

#**********************************************************************************************************************
#Class exercise - use dplyr to transform R (Richness) within the veg data to the square root (you've done this before)*
#**********************************************************************************************************************

#*********************************************
# PART 3: Manipulating and tidying data      *
#*********************************************

# So what is tidy data? Tidy conforms to the following simple rules:
# 1. One column per variable
# 2. One line per record or observation
# 3. One observation unit per table (e.g. counts of species in one experiment, measurements of stream water
# temperature in a riffle every 15 seconds)
# Read chapter 12 in Groelmund and Wickham's book:
# http://r4ds.had.co.nz/tidy-data.html#tidy-data-1

# load the library
require(tidyr)
# key functions in this library are:
# gather() 
# spread()
# separate()
# unite()

# Example of data with multiple variables in one column
# Create simple species by plot by sample "untidy" dataset to play with.....
Site <- c("Site1", "Site1", "Site1", "Site2", "Site2", "Site2", "Site3","Site3","Site3")
Count <- c(123, 156, 243, 533, 45, 4, NA, 45, 345)
Sample <- c("S1", "S1", "S1", "S2", "S2", "S2","S3", "S3", "S3")
Plot <- c("P1", "P1", "P1", "P2", "P2", "P2","P3", "P3", "P3")
Species <- c("Sp1", "Sp2", "Sp3", "Sp1", "Sp2", "Sp3","Sp1", "Sp2", "Sp3")

#create a dataframe
mydata <- data.frame(Site, Plot, Sample, Species, Count) # This is row(observation) -variable type data
mydata # see plot, samples and species are all in their own columns with a count.

# Now wrangle it so the species are assembled in columns (called Sp1 to Spn") and the count is each cell
MyColumnVarData <- spread(mydata, Species, Count)
MyColumnVarData

# Certain plot and analytical functions expect to see data in this format....so it's useful to know how to use it.
# This is the equivalent of pivot tables in Excel (but with knobs on!)

# gather() will push the data back to a longer format.

MyRowVardata <- MyColumnVarData %>%
  gather(Species, Count, c(Sp1, Sp2, Sp3))
MyRowVardata

# using separate() and other functions to tidy a messy dataset
# We will use a partial datafile on stream temperatures to illustrate this process

streamT<-read.csv("streamT.csv", header=TRUE, check.names=FALSE) 
# check.names=FALSE overrides R's formatting for column headings. It would bomb and complain about the headers without it.
#Column 1 is distance downstream in a river (m)
#Column 2 is depth in the stream bed (m)
#Columns 3:30 are dates and times of temperatures readings with T in (C)

# Have a look at these data - identify the issues.
str(streamT) 
head(streamT)
#Several of the columns are names are dates not numeric variables
#Stream temperature is hidden under the date columns making it difficult to analyse

#reformat the file so date is in one column with the relevant temperature reading per distance and depth.
streamTTidy <- gather(streamT, DateT, Temp, 3:29) 
# gathers columns 3:29 and places column headers into a DateT column and the temperatures into a Temp column

# remaining issue is that the DateT column has two variables date and time. We can resolve this using separate()
StreamTTidier <- streamTTidy %>%
  separate(DateT, into = c("Date", "Time"), sep = "\\ ") %>%
  separate(Date, into = c("Day", "Month", "Year"), sep = "\\/") %>%
  separate(Time, into = c("Hour", "Min"), sep = "\\:")

# Line 1 of this code takes the DateT column and pulls out Date from time using the separator \\ . This is a space.
# Line 2 separates Date into Day, Month and Year using the separate \\/
# Line 3 separates Time into Hours and Minutes using the separator \\:
# Check help file for more information on separators.
# But in short R is expecting the character that separates the fields to be given immediately after the \\
# You now have a tidy datafile to start analysis temperature change by distance downstream, depth into the stream bed
# and across time each day and or seasonally....

# We could have done it one larger code element using the piping operators to gather then separate

StreamTTidier2 <- streamT %>% gather(DateT, Temp, 3:29) %>% 
  separate(DateT, into = c("Date", "Time"), sep = "\\ ") %>%
  separate(Date, into = c("Day", "Month", "Year"), sep = "\\/") %>%
  separate(Time, into = c("Hour", "Min"), sep = "\\:")

View(StreamTTidier2)

#******************************************
# Data Wrangling Example from Brian McGill*
#******************************************

# Code derived from Brian McGill's blog - read them at:
# https://dynamicecology.wordpress.com/2016/08/22/ten-commandments-for-good-data-management/

# Work through the code line by line - it will REALLY help your understanding.....
# Some brill code snippets for correcting column names (you've done this) and also sorting typos!
# You'll need three datasets: raptor_survey.csv, raptor_sites.csv and raptor_temps.csv
# THESE FILES ARE IN YOUR DATA ZIP FILE ALREADY!!

# Load the holy trinity of data wrangling packages - thank you Hadley Wickham
require(dplyr)
require(tidyr)
require(lubridate)

# Step #1 - First load in files 
# These are the Raw Data files. 
# They follow a constellation schema per Commandment #3
#    (raptor_survey.csv/df is the fact table
#     raptor_temps.csv/temps is a 2nd environmental fact table
#     raptor_sites.csv/siteinfo is a dimension table (site) for both fact tables
# Two of the tables are in row-column form (per Command #2)
# But one table is in dimensional form rather than row-column. We will need to fix this

#bring things in NOT as R factors to start - just numbers or strings
options(stringsAsFactors = FALSE)
#have 3 files to load
df <- read.csv(file.choose(),header=TRUE) # raptor_survey.csv
temps <- read.csv(file.choose(),header=TRUE) # raptor_temps.csv
siteinfo <- read.csv(file.choose(),header=TRUE) # raptor_sites.csv

#
# Step #2 - explore what we have - determine dimensions, joining fields
#           check for errors
# This is part of commandment #4
#
str(df)  #hmm Alpha column/site has "1.o" instead of 1.0 (is chr not int)
head(df)
summary(df)
unique(df$spec)  #hmm Accipiter cooperii misspelled a bit
head(temps)
summary(temps)  #oops - 100 C is a bit hot
unique(temps$site)
head(siteinfo)  #column is "sitename" instead of "site"
unique(siteinfo$site)
summary(siteinfo)
View(df)
#
# Step #3 - clean up stuff (e.g. column names, things in single columns)
# Per commandment #4 we are doing cleaning up
# Per command #5 we are doing this in scripts and not hand editing the raw files
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# it is MUCH more repeatable to fix here in script 
# and leave raw files untouched
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#3a) site column called "sitename" in siteinfo inconsistent with "site" in core table
colnames(siteinfo)[[1]]="site"
str(siteinfo)  #names should match

#3b) fix string abunds for Alpha site
as.numeric(df$Alpha) #can't just coerce - gives warning
df$Alpha #lots of NAs are OK, #'s look OK
unique(df$Alpha[!is.na(df$Alpha)]) #zoom in on non-NAs
#ah ha somebody typed 1.o instead of 1.0
df$Alpha[df$Alpha=="1.o"]=1
#note other site columns are integer so convert to integer
df$Alpha=as.integer(df$Alpha) #no errors - nailed it
str(df) #OK all site columns numeric

#
# Step #4 - Make tall & filter
# Per commandment #2 we take our one table that is dimensional and make it row-column
#
# dplyr::gather([data],name_new_factor_column,name_new_value_column,cols to use or exclude)
dftall=df %>% gather(site,abund,-date,-spec)
dftall1=dftall %>% filter(abund>0) #drop all the NA rows


#
# Step #5 - more cleanup - again do in code, not raw files
# Commandment #4/5 again - I told you it was a large part of the work!
#

#5a fix cooperii misspelling
fix=which(dftall1$spec=="Accipiter cooperi")
cat("# of cooperi misspellings=",length(fix))
dftall1[fix,"spec"]="Accipiter cooperii"
unique(dftall1$spec)

#5b make a genus column
#separate(column,newcolnames,separater(default to non-alphanumeric))
dftall2= dftall1 %>%separate(spec,c("genus","specname"),remove=FALSE) %>%
  select(-specname)
df=dftall2 #save some typing

#Commandment #6
# We have a fact table (df) with 3 dimensions (date, site, species).
# we have another fact table (temps) with 2 dimensions (date, site)
# These share date and site dimensions
# The site dimension has its own dimension table (siteinfo) with additional site attributes


#
# Step #6 - join in ancillary tables to make one single 
#           "denormalized" data frame
# per comamndment #8, this is being done in reproducible code
#
#recall 2 tables to join:
#     siteinfo only on site
#     temps by site & date
#we are now working with factors, so clean up siteinfo

#do the first join to siteinfo
#beware - by= is one place you have to use quotes on column names
df1=full_join(df,siteinfo,by="site")
head(df1)
str(df1)

#OK now join in temps on 2 fields (site,date)
df2=left_join(df1,temps,by=c("site","date"))
head(df2,n=20) #rows 11 & 12 one site, one date, same temp


#
# Step #7 - now is right shape, make columns right type (should be last step)
# Per the spirit of Commandment #7 make this data as easy to use as possible
# per comamndment #8, this is being done in reproducible code
#
df2$date=ymd(df2$date)
df2$spec=as.factor(df2$spec)
df2$genus=as.factor(df2$genus)
df2$site=as.factor(df2$site)
df2$region=as.factor(df2$region)
df2$lon=as.numeric(df2$lon)
thedata=df2 #pull up to a name I'll remember for analysis
head(thedata)
str(thedata)

#
# Step #8 - add calculated columns, e.g DOY, Month of year
#
thedata=thedata %>% mutate(doy=yday(date),moy=month(date))

#
# Step #9 - Analyses
# Per commandment #9 our analyses are being done in reusable code
# Per commandment #8 we are conceptualizing this dimensionally,
#          and we are making this table completely denormalized
#          and as easy to work with (adding calculated fields like DOY)
#          but also per commandment #8 we are leaving this in the native
#          form for our analysis tool, R and dplyr, namely row-column data
#
# a) filter to april (full month surveyed) & summarize by doy
thedata %>% filter(moy==4) %>% group_by(doy) %>% summarise(abund=mean(abund,na.rm=TRUE))
# b) same but doy and region
thedata %>% filter(moy==4) %>% group_by(doy,region) %>% summarise(abund=mean(abund,na.rm=TRUE)) %>% spread(region,abund)
# c) top 5 counts
thedata %>% top_n(5,abund)
# d) summarize to views per month, normalized by days observed
thedata %>% group_by(moy) %>% summarise(abund=sum(abund,na.rm=TRUE),obsdays=n()) %>% 
  mutate(abund.per.day=abund/obsdays) %>% select(moy,abund.per.day)
# e) summarise to month  x species table (normalized by days observed)
thedata %>% group_by(moy,spec) %>% summarise(abund=sum(abund,na.rm=TRUE),obsdays=n()) %>% 
  mutate(abund.per.day=abund/obsdays) %>% select(moy,spec,abund.per.day) %>%
  spread(moy,abund.per.day)
# f) summarize to views per month each year, normalized by days observed
thedata %>% group_by(moyr=floor_date(date,"month")) %>% summarise(abund=sum(abund,na.rm=TRUE),obsdays=n()) %>% 
  mutate(abund.per.day=abund/obsdays) %>% select(moyr,abund.per.day)


# **********************************************************   
# CLASS EXERCISES:  Importing and manipulating data frames *
# **********************************************************
# 1. Use the deep sea research data (ISIT.txt). Load it in using read.table. The file contains bioluminscence data oo
# organisms from various depths and locations in the North Sea. Repeat using tidyverse packages.

# 2. Extract the data from station 1. How many observations are there from this station?

# 3. What are minimum, maximum, median and mean sampled depth of stations 2 and 3.

# 4. Identify stations with fewer observations than 20. Create a data frame omitting them.

# 5. Extract the data for 2002 and sort it by increasing depth values.

# 6. Show the data that were measured at depths greater than 2000m in April (all years).

# 7. Check out the merge() function that allows you to merge datasets with a common ID. 


