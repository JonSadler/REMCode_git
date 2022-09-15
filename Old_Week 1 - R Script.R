# *************************************************************************
# This is the source script for WEEK 1: An introduction to R and R Studio *
# *************************************************************************
# Jon Sadler Sept 2013 updated Jan 2017

#  **********************************
#  ****** FIRST IMPORTANT TASK ****** 
#  **********************************
# Create a directory on your desktop called 'RWork' (or whatever you want) and download the data and script files from the Canvas website)

# Note the colour coding on the scripts (using indents to tidy your code also helps; especially with programme loops)
# This character '#' indicates a comment and can be placed above your code to indicate what's coming next or adjacent to the code as indicated below:
# Data sets are derived from Zuur et al. Beginner's Guide to R.

# *************************************************************************
# IMPORTANT TASK
# I have set up a group document in the canvas pages to allow you to edit and list and describe the functions
# you have used in each session. Remember to update it every week!!!
# *************************************************************************

# Well start with a simple function

rm(list=ls())

# Clears the R brain. Why do this? Because R holds onto objects in the workspace. It can lead to issues with variable names and the 'attach' function.

# **************************
# Setting the home directory and environment

# ********************************************************************************************************
# You can do this in RStudio by selecting "Session" then "Set Working Directory" then "Choose directory" *
# I'll demonstrate that just before we start with the coding
# ********************************************************************************************************

# Environment setting to ensure you have rights to install a library on a network computer
.libPaths("C:/Program Files/R/R-3.3.0/library")
# NOTE: this is not needed on a standalone machine and/or laptop. The permissions will be fine.

#Check where R is looking
getwd()

# **************************
# Using R within RStudio   *
# **************************
citation()          # Use this to figure out what version or R you're using. Cite this in your project/paper
citation("lattice") # Find the citation for the packages you use in R. It failed when you ran it because the package wasn't loaded 
                    # Let's load it now and repeat the process. Notice the need for the quotes ""!!!!!
library(lattice)    # Loads a library or package. This was offers advanced visualisation / graphics and we'll look at it later in the course

# *********************************************
# Part 1: Getting Data into R via the Console.*
# *********************************************
# Most of the following derives from Zuur et al. 2009 Beginner's Guide to R. Read it (or buy it)
# First Steps - typing in a small dataset

a <- 59
b <- 55
c <- 55.5
d <- 55

d
a

# Now use some basic maths operators to play around with them.
# a * b etc
# To see what you've done type any of the letters. You could have called these variables anything 
# but it is wise to use names that are memorable and relate to what the variable represents. So:

Wing1 <- 59         # Note naming conventions in R. It's case sensitive, no spaces, forward slashes,
Wing2 <- 55         # hyphens; the names cannot start with a numeric either!
Wing3 <- 55.5       # It will accept dots (periods), underscores etc
Wing4 <- 55         # Capitalising variable names is good practice as there maybe functions with similar names!
Wing5 <- 52.5

# ***************************
# Base R is powerful and has numerous functions allocated to it. Let's try a few basic ones with these data. 
sqrt(Wing1)           # Square root of variable Wing1
2 * Wing3
Wing1+ Wing2+ Wing3
Wing1 * Wing5 / 5

# Although R is performing these calculations and returning the answer via the console, it is not storing them.
# To store these you need to define new variables (or objects)
SQ.Wing1 <- sqrt(Wing1)       # Calculation product is now stored as another variable
SQ.Wing1                      # Type the variable name to display the answer
x2Wing3 <- 2 * Wing3
x2Wing3

# ***************************
# Using the concatenate function to add multiple values to a variable, creating a vector. This is a REALLY useful function
# for annotating and adding variables to data frames. Learn how to use it.
Wingcrd <- c(59, 55, 53.5, 55, 52.5, 57.5, 53, 55)      # Note spaces improve readibilty and you need () brackets not
Wingcrd                                                 # [] or {}; these are reserved for other things.
                                                        # If you are using strings then remember c("str1", "str2"). It   
                                                        # needs quotation marks " "

# Once you have data in a vector you can find individual values and ranges of values
Wingcrd [1]         # Finds the first value
Wingcrd [1 : 5]     # Returns values 1-5
Wingcrd [-2]        # Returns all values excluding value 2

# Now we have multiple variables we can use base R functions to provide statistical descriptors of them (sum, mean, max, min, median, var, sd and so on)

sum.wings <- sum(Wingcrd)         # We have allocated the sum of the values to the variable sum.wings
sum.wings

#**************************************************************************
# try a few more out for 5 minutes or so. Use the mean, sd, var functions * 
#**************************************************************************

# Now let's add the other variables in same way to create the other variables in the table
Tarsus <- c(22.3, 19.7, 20.8, 20.3, 20.8, 21.5, 20.6, 21.5)
Head <- c(31.2, 30.4, 30.6, 30.3, 30.3, 30.8, 32.5, NA)
Wt <- c(9.5, 13.8, 14.8, 15.2, 15.5, 15.6, 15.6, 15.7)

# Note one variable has a missing value; these must be listed with an NA; empty cells are a problem!
# Missing values cause issues for some functions; 
sum(Head)       # Told you!

# it's wise not have any but if you do, it's no big deal.
sum(Head, na.rm = TRUE)         # Setting this to TRUE gets around the issue. Note check help files for functions
                                # Some use 'na.action'


# Let's pose a question. Can you combine these variables Wingcrd, Tarsus, Head, Wt into one file?
BirdData <- c(Wingcrd, Tarsus, Head, Wt)
# Let's have a look 
BirdData

# hmmm big mess!!!! BirdData is a single vector of length 32 (4 x 8). It is organised this way because it cannot distinguish the 8 values in each variables. To sort this out we need to create another variable with 32 values:
ID <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
        4, 4, 4, 4, 4, 4, 4, 4)

# Now consider what would happen if the vector was 100 or 1000 digits long. It would be a task! R has a function to help 'rep'
ID <- rep(c(1, 2, 3, 4), each = 8)  # So this tells R to repeat the creation of 4 vectors 8 times in sequence
ID                                  # What's it look like
# Or we can do it another (always the case in R!!!!) by creating sequence allocating that to another variable (in this case 'a')
a <- seq(from = 1, to = 4, by = 1)  
a                                   # See.....
ID <- rep(a, each = 8)              # Then we use 'rep' again            
ID                                  # Same result!!!!
BirdData1 <- c(ID, Wingcrd, Tarsus, Head, Wt)
BirdData1
# This all seems a bit pointless but R needs data in different formats and long vectors are needed for somethings
# The rep function and the ID variable allows you to identify groups of observations to the morphology variables

# But sometimes we need data in a table. So we can bind these values into a data file of sorts
BirdData2 <- cbind(Wingcrd, Tarsus, Head, Wt)
BirdData2                                # See product of your labour. A data file!

# Suppose now you want to access elements of BirdData2 to check on data accuracy
# NOTE the format of the subset search here [row,column]. You need to tell R to look in the right place
BirdData2[,1]                       # Lists ALL the data from the first column
BirdData2[1,]                       # Lists ALL the data from the first row
BirdData2[1,1]                      # Reports the item in the first row and column
BirdData2[,2 : 4]                   # Reports the data in columns 2-4 ;"Tarsus", "Head" and "Wt"
BirdData2[,-1]                      # Returns all columns minus column one "Wingcrd)
BirdData2[, c(1, 2, 4)]             # Returns all the data from columns 1, 2 and 4 (i.e. non sequential)

# If you've forgotten how many rows and columns you can use the dim function (reports rows by columns)
dim(BirdData2)
# ****** REMEMBER FOR ALL CALL OPERATIONS ROW FIRST, COLUMN SECOND ******

# **************************
# So that's a lot of work for a simple data file we could (and will later) load in from a text file or excel 
# Why do it? Because you've learned some very important functions 'c', 'rep' and  'cbind' are three of my favourites (but check 'rbind')
# Also functions in R require data in different formats. Tables (known as wide' format) are needed for multivariate analyses but 
# data organised in longer vector with a unique (ID) is needed for others (known as long format), such as T-test, ANOVA, linear regression 
# and graphing tools like xyplot in the lattice package. We'll looks at ways of doing this in R later (using ddply and tidyR).

# Now let's look at creating a data frame. They have special properties and are the general means of housing data in R (after import). 
# So we can recreate the data file above using the data.frame function (we'll shorten the names as well).

BirdData3 <- data.frame(WC = Wingcrd,                   
                        TS = Tarsus,                    
                        HD = Head, 
                        W = Wt)
BirdData3

# This may seem unnecessary but data frames allow you to combine and modify data so they are much flexible

# *************************************************************************
# IN FACT YOU'LL USE DATAFRAMES ALMOST TO THE EXCLUSION OF OTHER DATATYPES*
# BUT check datatable for a faster more flexible version                  *
#**************************************************************************

# Some key things you need to know about dataframes:
# They hold data of different types unlike matrices and vectors more easily
# No real limit on size (for most purposes) but bigger does = slower
# They allow you to draw out data with all the available filters and searches (see above)
# You can use the $ operator to pull out one individual column. So the $ operator assigns a column
# to the data frame

BirdData3				# returns the full dataframe in its entirety, whereas:

BirdData3$WC			# returns just that one column

# You can also add in additional columns. Here we are adding another (transformed) variable to the frame (with no issues)
BirdData4 <- data.frame(WC = Wingcrd,                   
                        HD = Head, 
                        W = Wt,
                        Wsq = sqrt(Wt))
BirdData4

# VERY useful!!! More on this later......

# Add this point you can also rename either the column headings and the row headings
# using the colnames() and rownames() functions. Have a look at this using the ?? help from 
# the console window / pane (if you are in RStudio)

#**********************************************************************
# a Few more key functions - will be useful as you progress           *
# *********************************************************************
# read.table imports data from a text file
# names() - lists the column names in the datafile
# str() shows the data structure of the datafile.
# dim() gives the fuile dimensions (rows v columns)
# head() displays the first six rows
# tail() displays the last six rows
# View() displays 1000 rows of the datafile (like a spreadsheet)

# The 'table()' function returns information on the length or number of observation on a dataframe variable
# It's very useful for understanding the data balance in the design and possible interactions (and issues about those)
# Start by importing the deer.txt dataset again; use read.table. Go with it for the time being....we are going to follow this up next week with more om importing data.

Deer <- read.table(file = "Deer.txt", header = T) # R won't see this unless it is in your home directory!
names(Deer)
str(Deer)  # The variable Farm is a string so was imported as a factor. 
dim(Deer) #1182 rows and 9 columns
head(Deer) 
tail(Deer)
View(Deer) #NOTE - this is an RStudio function call so notice it is capitalised....

# If we want to how many observations we have per farm, then:
table(Deer$Farm)

# We can see that a few farms have got very few observations (i.e. it's unbalanced) - we'd need a mixed model to analyse it
# We are now interested to see whether gender interacts with year

table(Deer$Sex, Deer$Year)
# Rows: 1 = Males, 2 = Female
# Columns: 0 = 2000, 1 = 2001, 2 = 2002, 3 = 2003, 4 = 2004, 5 = 2005 and 99 = 1999
# In 1999 no females were sampled, so you cannot use an interaction effect for Year:Sex in your models. This will make sense later in the course!


# ****************************************************
# EXERCISE 1 - using the c function to create vectors*
# ****************************************************
# Use the following data
#   Farm    Month   Year    Sex   LengthClass   LengthCT    Ecervi    Tb
#   MO      11      00      1     1             75          0         0
#   MO      07      00      2     1             85          0         0
#   MO      07      01      2     1             91.6        0         1
#   MO      NA      NA      2     1             95          NA        NA
#   LN      09      03      1     1             NA          0         0
#   SE      09      03      2     1             105.1       0         0
#   QM      11      02      2     1             106         0         0

# These data are a small part of a data file used in Zuur et al. 2009 ( = deer.txt for full data)
# Tb is the occurrence of Tuberculosis in cattle on some farms;
# The variables Farm, Month, Year, Sex, LengthClass are self-explanatory
# Ecervi is the occurrence of a cattle parasite

# Tasks:
# 1. Using the c function, create a variable that contains the length variable that contains the lengths of seven animals
# 2. Also create a variable that contains the Tb values
# 3. What's the average length, sd of the length of the cattle on the farms?

#*****************************************************************************
# CLASS EXERCISE 2: Using BirdData2 find the following using the [] functions* 
#*****************************************************************************
# 1. data found in the cell situated in the last row and last column
# 2. rows 3-4 in column called "Head"
# 3. All the data from columns 2 and 3 (you need to use 2: 3 to link the columns)
# 4. All the weights and write them to a variable Y
# 5. All the columns excluding the first and third "Wingcrd" and "Wt"; use the 'c' function to do this. Write them to variable X

# *************************************************
# CLASS EXERCISE 3: Creating and using data frames*
# *************************************************
# 1. Use the data on deer parasites and Tb (EXERCISE 1 above) to create a data frame
# 2. Square root transform the length variable
# 3. Add it to the data frame as a new variable
# NOTE: To do this you need to use 'c' to create all the variables first!!!

# **** IF YOU'VE GOT HERE AND STILL HAVE TIME LEFT MAKE A START ON WEEK 2!!!! ****
