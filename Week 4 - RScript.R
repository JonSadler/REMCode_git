# ****************************************************************************
# This is the source script for WEEK 4: Comparing groups                     *
# ****************************************************************************
# Jon Sadler Feb 2014 updated Feb 2021

# Create a directory on your desktop called 'RWork' (or whatever you want) and download the data and script files from the Canvas website)
# Spend the some time reviewing last weeks material....!!!!

# **********************************************************************************************************************
# Alternatively you can do this in RStudio by selecting "Session" then "Set Working Directory" then "Choose directory" *
# **********************************************************************************************************************

# INTRODUCTION - Means and variability (see excellent chapter in Crawley's book on this). 
y <- c(13, 7, 5, 12, 9, 15, 6, 11, 9, 7, 12) # create some basic data
plot(y, ylim = c(0, 20))    # plot it as a scatter limiting the axis 0 - 20

# How can we quantify this scatter? We could use the range
range(y)

# But we need something standardised against the mean...

mean(y)

y - mean(y)  # The result shows both negative and positive values (not ideal when we sum it)

# So we square the sum to get the 'sum of squares'
sum((y-mean(y))^2)

# but this value is going to increase everytime we add to it!
# So we need to standardise it by dividing the value by the number of samples

sum((y - mean(y))^2)/(length(y) - 1) # Notice we are subtracting one from the number of samples = the degrees of freedom

# or we can just call the var() function
var(y)

#************************************************
# PART 1: Comparisons of two independent samples*
# t-Tests                                       *
# use ozone.csv                                 *
#************************************************

ozone <- read.csv(file.choose())

# look at it
str(ozone)
head(ozone)

# We wish to know if the two gardens differ in their ozone concentrations
# We can use a histogram to show this; it will illustrate the central tendencies of the data

# But first we need to subset the data to pull out the bits we need for the comparison. We did this last week with the same data
GardA <- ozone[ozone$Garden == "A", ]

# And the same using the subset function 
GardB <- subset(ozone, Garden == "B")

# Make a picture and check the assumptions
par(mfrow = c(2, 1))

#Add histograms with a fixed range for the X axis
hist(GardA$O3, xlim = c(0, 7), main = "", col = "Darkblue", xlab = "Garden A") 
hist(GardB$O3, xlim = c(0, 7), main = "", col = "Lightblue", xlab = "Garden B")

# turn off former graphics device
dev.off() 

# CLASS Exercise 1 MORE assumption checking....(10 mins max) 
# 
# (i) plotting a qqplot (use help ?qqplot)# (ii) plotting a QQPlot READ the help file!
qqnorm(ozone$O3); qqline(ozone$O3)

# (ii) using the shapiro-wilks test (use the help system ?shapio.test). READ the help file!

shapiro.test(ozone$O3)

# (iii) calculating the garden variance

# You've already done this.

# (iv) test for heterogeneity of variances using levenes test (You need to load the 'car' library first; then select ??leveneTest)

library(car)
leveneTest(O3 ~ as.factor(Garden), data = ozone)

# Run a t-test
t.test(GardA$O3, GardB$O3)

# Sometimes it makes sense to store the response variable in one column - so in long format or row-column...
# With an addition column that identifies the groups
# When data are in this format then we need a different test syntax
# This is what we have in our data anyway - look at it
ozone

# t-test with using a grouping factor
t.test(O3 ~ Garden, data = ozone) # Note - the data argument and '~' (tilde) operator (you'll see a lot of this)

# U-Tests (Wilcoxon tests)
# Use these if your variables are not normally distributed or if variances are not equal
wilcox.test(O3 ~ as.factor(Garden), data = ozone)

# We'll ignore the error for the time being - it is suggesting the use of 
# a permutation approach perhaps using the 'coin' package see code below.
# Exact Wilcoxon Mann Whitney Rank sum using the coin package
# Use where y is numeric and x is binary factor
library(coin)
wilcox_test(O3 ~ as.factor(Garden), data = ozone, distribution = "exact")

# note we are forcing the garden into a factor as it is currently a character field.

#********************************************************
# PART TWO: Comparisons of more than two samples        *
#********************************************************

# Introduction to linear models
# We'll be using the aov() function to do most of this (both both lm and glm will work)

# Single factor Anova
# Create the data
H <- c(1,2,2,5,6,5,2,1)   					# Create samples
Sam <- c(1,1,1,2,2,2,3,3) 					# Categorize samples
A.test <- data.frame(H, Sam)				# Create data frame
A.test$Sam <- as.factor(A.test$Sam)			# Make sure sample is a factor - remember we entered as a vector of integers

# Check the data
str(A.test)
A.test

# Draw some pictures
hist(Sam, xlab = "Sample", main = "", col = "grey")

#(iii) Use a boxplot to look at the data structure - Look at the medians in samples 1 and 2!
boxplot(H ~ Sam, data = A.test, xlab = "Sample", ylab = "Number")

# CLASS EXERCISE 2: 
# (i) Check for normality, histogram, QQplot, Shapiro-Wilks test
# (ii) Check for heterogeneity of variances using a levene test. NOTE there are graphical means of doing this....more next week
# NOTE: There is considerable debate over normality tests (see: http://www.r-bloggers.com/normality-tests-don%E2%80%99t-do-what-you-think-they-do/)

# Run single factor ANOVA
Anova.run <- aov(H ~ Sam, data = A.test)

# Check results
summary(Anova.run)

# Our boxplot indicated a large difference between sample 2 and others
# we can examine the difference between the group means using a posthoc test 
# such as a Tukey test - TukeyHSD(add in anova object name)

TukeyHSD(Anova.run)

# Comparisons of more than two non-independent samples

# Kruskal-Wallis tests

KW.test <- kruskal.test(A.test$H ~ A.test$Sam)

# Look at the results
KW.test 


#********************************************************
# PART THREE: 2-way and factorial ANOVA                 *
#********************************************************

# Load in data (datafile  = Grazing.csv)
Graze <- read.csv(file.choose())

# Look at it
Graze

# Check the other commands str()
str(Graze)						# look at the structure
# Notice that the string data (cols Field and Grazing has been imported as characher data not as factors.
# Some functions will interpret the field as facrors other won't.

#••• !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!••••••••••••••••••••
# IMPORTANT FIRST TASK IS ESTABLISHING WHETHER YOUR ANALYSIS IS BALANCED •
# THIS IS CRUCIAL WHEN USING THE aov and lm() functions   
#  WHERE YOU ARE COMPARING TWO FACTORS                                   •
#••• !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! •••••••••••••••••••

# Check this using the table function. 
table(Graze$Field, Graze$Grazing)

# You can also 'test' for this using the replications function
replications(Abund ~ Grazing * Field , data = Graze)
!is.list(replications(Abund ~ Grazing * Field , data = Graze)) # We're safe !!!!

# Draw some pictures - class exercise (10 mins) 

hist(Graze$Abund)
Field1 <- subset(Graze,Field == "Lower")
Field2 <- subset(Graze, Field == "Top")

# Compare fields
par(mfrow = c(2, 1))

#Add histograms with a fixed range for the X axis
hist(Field1$Abund, xlim = c(0, 40), main = "", col = "Darkblue", xlab = "Field 1") 
hist(Field2$Abund, xlim = c(0, 40), main = "", col = "Lightblue", xlab = "Field 2")

# Boxplots to look at difference of factors
par(mfrow = c(2, 1))
boxplot(Abund ~ Field, data = Graze)
boxplot(Abund ~ Grazing, data = Graze)

# turn off former graphics device
dev.off() 

# We can also combine the factor as an interaction. NOTE the * symbol indicates the interaction
boxplot(Abund ~ Field * Grazing, ylab =  "Abundance of Rye Grass", xlab = "Contrasts of field and grazing level", data = Graze)

# Coplot to look at the interaction of the factors (with added smoother line)
coplot(Abund ~ as.factor(Grazing) | as.factor(Field), panel = panel.smooth, xlab = "Grazing Level", ylab = "Rye grass abundance", data = Graze)
# Notice:
#1.we are coercing the character fields Grazing and Field as factors.The coplot function requires this.
#2. how R orders the factors alphabetically on the X axis. Unhelpful because the (High, Low, Mid) are relational to each other - indicating a known order 
# Compare the means and errors on a line plot
# To do this we need to summarise the data to get the means and standard errors
# There are numerous ways of doing this (tapply etc) but we are going to do it using a package called dplyr
# You'll need to install it as it won't be there......

# Reorder the categorical variable. R plots factorial variables alphabetically - not useful when your factors relate to a quantity of something!!!!
Graze$Grazing <- factor(Graze$Grazing, levels = c("Low", "Mid", "High"), ordered = TRUE)

# Run the two-way ANOVA
Grazeaov <- aov(Abund ~ as.factor(Field) + as.factor(Grazing), data = Graze)
summary(Grazeaov)

# Let's check some assumptions using R's in-build plot functions....
# these do the same thing as your tests (e.g. Levenes) and QQ-plots

#YOU WILL BE USING THIS COMMAND ALOT. PLEASE READ (COPY AND PASTE INTO YOUR BROWSER):
# https://www.oxfordscholarship.com/view/10.1093/acprof:oso/9780198787839.001.0001/acprof-9780198787839-chapter-5
# This whole book is a HUGE must read - we have an online library subscription (check the resources tab on the CANVAS page):
# Getting Started with R: An Introduction for Biologists
# Andrew Beckerman, Dylan Childs, and Owen Petchey

par(mfrow = c(2, 2)) # plot four pictures per page
plot(Grazeaov)
# set the device off....
dev.off()


# Look okay apart from plot 4 which indicates a lumped pattern.
# Now let's try the interaction between the two factors to see if it improves the fit.

Grazeaov1 <- aov(Abund ~ as.factor(Field) * as.factor(Grazing), data = Graze)
summary(Grazeaov1)

# We can visualise the interaction plot using the interaction.plot function
interaction.plot(Graze$Grazing, Graze$Field, Graze$Abund, col=c(2,3), xlab = "Grazing Regime", ylab = "Rye Grass abundance", trace.label = "Field")

# To repeat the interaction plot in ggplot2 you'll need to generate some summary data (mean, and standard error (se))
# Have a go at using summarise in dplyr to generate summary data on the two treatments call it output Grazesum 
# (or you'll overwrite your datafile!). You need to group_by factor and summarize for the mean, sd, 
# count the number of samples (using length()) and generate the se using sd/sqrt(N)

library(dplyr)
Grazesum <- group_by(Graze, Field, Grazing) %>% summarise(mean = mean(Abund),
													   sd = sd(Abund),
													   N = length(Abund),
													   se = sd/sqrt(N))
													   
library(ggplot2) # load ggplot2

pd <- position_dodge(.3)
ggplot(Grazesum, aes(x = Grazing, y = mean, colour = Field, group = Field)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, size = 0.25, colour = "Black", 
                position = pd) + geom_line(position = pd) + geom_point(position = pd, size = 2.5)

# Let's check some assumptions using R's in-build plot functions....
par(mfrow = c(2, 2)) # plot four pictures per page
plot(Grazeaov1)
# set the device off....
dev.off()

# CLASS EXERCISE - confirm normally and homogeneity of variance with shapiro and levene's tests
# (5-10 mins)


#•••••••••••••••••••••••••••••••••••••••••••
## PART FOUR: CLASS EXERCISES               •
#•••••••••••••••••••••••••••••••••••••••••••

# Do the following for all data files:

# 1. Load in the data and examine it's structure (including experimental balance)
# 2. Posit your hypotheses (null and alternative)
# 3. play with some pictures
# 4. draw a contingency table to figure out how the data are structured
# 5. Select an appropriate ANOVA-based model (NOTE: we have more than 2 samples...so no t tests!)
# 6. Validate the model
# 7. Briefly interpret the results - in your script file!!!!

# EXERCISE 1
# -----------------------------------------------------------------------------------------
# Filename: Hoglouse.csv - a file of water louse distribution along a rivers in Devon
# response - hoglouse numbers
# explanatory variable - Upper, Mid and lower sites (i.e.  longitudinal profile)
# see Gardner's book....on the reading lists
# ----------------------------------------------------------------------------------------

# EXERCISE 2
# -----------------------------------------------------------------------------------------
# Filename: Medley.csv
# Medley and Clements (1998) investigated the impact of zinc contamination (and other
# heavy metals) on the diversity of diatom species in the USA Rocky Mountains (from
# Box 8.1 of Quinn and Keough (2002))
# File contents:
# DIATOM - number of different species of diatoms on the a known area of rocks in streams (continous variable)
# ZINC - mpm of zinc in the water column (background, low, medium, high) (factor - explanatory variable)
# ----------------------------------------------------------------------------------------

# EXERCISE 3
# -----------------------------------------------------------------------------------------
# Filename: Quinn.csv (data from By G. P. Quinn and M. J. Keough, 2002 - "Experimental Design and Data Analysis for Biologists" )
# File contents:
# DENSITY - urchin density treatment (L1 = 8 individuals per 225 cm2 enclosure, L2 = 15, L3 = 30 and L4 = 45) (factor - explanatory variable)
# SEASON - Season of the year (Spring or summer) (factor - explanatory variable)
# EGGS - egg production by limpets (continous response variable)
# ----------------------------------------------------------------------------------------

# EXERCISE 4 [WILL BE YOUR FORMATIVE EXERCISE]
# -----------------------------------------------------------------------------------------
# Filename: fish_pred.csv (data from Doncaster and Davey, 2002, p. 50)
# fish predation experiment - using enclosures of two species of fish to assess their impact on predation on chironomids
# File contents:
# Density - density of chironomids left activty fish predation exercise (response variable - continuous)
# Loach - Presence of a loach in the enclosure (factor - 0 = absent, 1 = present)
# Bullhead - Presence of a bullhead in the enclosure (factor - 0 = absent, 1 = present)
# ----------------------------------------------------------------------------------------
