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

