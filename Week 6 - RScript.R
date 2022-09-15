# ******************************************************************************
# Source script for WEEK 6: Multiple Linear Regression and model selection     *
# ******************************************************************************
# Jon Sadler Mar 2015 updated Feb 2021

# Set the home directory


# *******************************************************************************************************************
# Alternatively you can do this in RStudio by selecting "Session" then "Set Working Directory" then "Choose directory" *
# *******************************************************************************************************************

# ***********************************************
# PART ONE: Additive Multiple linear regression *
# ***********************************************
# First an additive model....no interactions but with some simple model selection...the old way

#load datafile loyn.csv - call it Loyn or the rest of the code won't work!!!!

Loyn <- read.csv(file.choose())

# Look at its structure and shape....you know the pertinent function calls!!

# Response (y) variable is ABUND - bird abundance per patch
# Explanatory - AREA - forest patch area
# Explanatory - YR.ISOL - Year the forest patch was isolated
# Explanatory - DIST - Distance in km to nearest forest patch
# Explanatory - LDIST - Distance in km to nearest large forest patch area
# Explanatory - GRAZE - Grazing intensity in the patch
# Explanatory - ALT - Altitude of the patch

# 1. Check for normality - homogeneity etc
# We're going to do this in one massive sweep using the car package

require(car)
scatterplotMatrix(~ABUND + AREA + YR.ISOL + DIST + LDIST + 
	GRAZE + ALT, data = Loyn, diag = list(method = "boxplot"))
# This code creates a huge matrix of the variables compared against each other with a
# boxplot in the middle showing the distribution of each variable...
# It's a bit confusing so study it carefully (5 mins)
# Then we'll discuss it....
# IF YOU GET A MARGIN ERROR MAKE YOUR SCREEN FULL SIZE AND INCREASE THE SIZE OF
# THE PLOT WINDOW!!

# Okay so let's repeat this with some transformationsrequire(car)
scatterplotMatrix(~ABUND + log10(AREA) + YR.ISOL + log10(DIST) + log10(LDIST) + 
	GRAZE + ALT, data = Loyn, diag = list(method = "boxplot"))

# 2. Check for multicollinearity
# First by correlation
cor(Loyn[, 2:7])		# select columns 2 - 7 all the explanatory variables!
# We are looking for correlation coefficients > 0 .5. Only YR.ISOL and GRAZE have this property

# Then use Variation inflation Factors (VIFs) and Tolerances to examine whether they'll impact the linear regression
vif(lm(ABUND ~ log10(AREA) + YR.ISOL + log10(DIST) + log10(LDIST) + 
	GRAZE + ALT, data = Loyn))
# We want our VIFs to be less than 3

# SO we have no collinearity issues despite the correlation coefficients. We can proceed with the modelling:

Loyn.lm <- lm(ABUND ~ log10(AREA) + YR.ISOL + log10(DIST) + log10(LDIST) + 
	GRAZE + ALT, data = Loyn)
	
# Validate the model
op <- par(mfrow = c(2,2))
plot(Loyn.lm)
par(op)

# In ggfortigy
library(ggfortify)
autoplot(Loyn.lm)

# The QQ-plot isn't great here nor the residual v fitted but Logan (2013) thinks they're okay
# We'll accept them for the time being 
# The neater way is to model it differently - but not right now!

# Check residuals against explanatory variables
op <- par(mfrow = c(3,3))
plot(Loyn.lm$resid ~ log10(Loyn$AREA)) 	# Looks okay
plot(Loyn.lm$resid ~ Loyn$YR.ISOL)		# Looks okay
plot(Loyn.lm$resid ~ log10(Loyn$DIST))	# Looks okay
plot(Loyn.lm$resid ~ log10(Loyn$LDIST))	# Looks okay
plot(Loyn.lm$resid ~ Loyn$ALT)			# Looks okay
boxplot(Loyn.lm$resid ~ Loyn$GRAZE)		# Don't like the look of this one. We can deal with another way - but not today
par(op)

# We assume everything is good (as the published paper indicated) and look at the results
summary(Loyn.lm)

# To finish we plot the slope partials....
avPlots(Loyn.lm)			# This is a call to the car package

# This confirms that AREA is important (has a slope); the rest are virtually flat!

# ***********************************************
# PART TWO: Multiplicative linear regression    *
# ***********************************************

# Load datafile pareulo.csv
# look at it using the familiar commands and try dim() to give you the number of columns (variables)
# And samples (rows)

# Response (y) variable is C3 - relative abundance of C3 grassland plants
# Explanatory - MAP - Mean annual precipitation 
# Explanatory - MAT - Mean annual temperature
# Explanatory - JJAMAP - Proportion of annual mean ppt June - August
# Explanatory - DJFMAP - Proportion of annual mean ppt December - February
# Explanatory - LONG - longitude 
# Explanatory - LAT - latitude

# Paruelo, J.M. & Lauenroth, W.K. (1996) Relative abundance of plant functional types in grasslands and shrublands 
# of North America. Ecological Applications 6: 1212–1224.
# 73 different sites covering all of the USA (3 sites in Canada)

Pareulo <- read.csv(file.choose())
# Normality, linearity and homogeneity checks
require(car)
scatterplotMatrix(~ C3 + MAP + MAT + JJAMAP + DJFMAP + 
	LONG + LAT, data = Pareulo, diag = list(method = "boxplot"))
# Normality looks okay for explanatory variables but the C3 response is skewed
# suggest a scalar tranformation on that first - log(C3 + 0.1) - there are zeros and you cannot log a zero!
# So we use a small constant - in this 0.1
# Explanatory / Response relationships indicate non-linearity - we might need to log them
# Some explanatory variables are intercorrelated

# So we log the response and repeat...
scatterplotMatrix(~log10(C3 + 0.1) + MAP + MAT + JJAMAP + DJFMAP + 
	LONG + LAT, data = Pareulo, diag = list(method = "boxplot"))

# Linearity still an issue for explanatory variability but we'll check that in the model residuals

# 2. Check for multicollinearity
# First by correlation
cor(Pareulo[, 2:7])		# select columns 2 - 7 all the explanatory variables!
# We are looking for correlation coefficients > 0 .5.
# Latitude and longitude are an issue
# As are some of the climate variables

# Then use Variation inflation Factors (VIFs) to examine whether they'll impact the linear regression
vif(lm(log10(C3 + 0.1) ~ MAP + MAT + JJAMAP + DJFMAP + 
	LONG + LAT, data = Pareulo))
# We want our VIFs to be less than 3. Yikes huge issues with LAT and LONG and some climate parameters

# Solution is to remove the offending variables. We can start with LAT and LONG
# because climate variables are always correlated to geographic distance
vif(lm(log10(C3 + 0.1) ~ MAP + MAT + JJAMAP + DJFMAP, data = Pareulo))

# Much better but still the two summer and winter ppt figures are a problem (they are intercorrelated)
# Use some ecological thinking here. summer ppt should be more important to C3 plants so remove DJMAP
vif(lm(log10(C3 + 0.1) ~ MAP + MAT + JJAMAP, data = Pareulo))

# Now we are good to go.....we are planning to look for interactions so the full model is:
M1 <- lm(log10(C3 + 0.1) ~ MAP * MAT * JJAMAP, data = Pareulo) # full model
# Look at it...
summary(M1)
# No significant patterns - needs simplification so start by removing interaction terms that are not sign:

M2 <- lm(log10(C3 + 0.1) ~ MAP + MAT + JJAMAP+ MAP:MAT + MAP:JJAMAP + MAT:JJAMAP, data = Pareulo)   # remove 3-term first
summary(M2)

M3 <- lm(log10(C3 + 0.1) ~ MAP + MAT + JJAMAP+ MAT:JJAMAP, data = Pareulo) 	# Remove non-sig. 2-way terms
summary(M3)

M4 <- lm(log10(C3 + 0.1) ~ MAP + JJAMAP+ MAT:JJAMAP, data = Pareulo) 	# Remove least significant main term MAT
summary(M4)

M5 <- lm(C3 ~ MAP + JJAMAP+ MAT:JJAMAP, data = Pareulo) 	# Remove least significant main term MAT
summary(M5)

autoplot(M5)
autoplot(M4)
# NOTE: it's easier to use the update function do this (have a look at it); I did it long hand so you could see
# what was chopped out.

# This is our best model...check assumptions
autoplot(M4)

op <- par(mfrow = c(2,2))
plot(M4)
par(op)
# These look okay but scale - location is slightly humped. Made worse by red line ; points look okay
# plot residuals against explanatory variables
op <- par(mfrow = c(2,2))
plot(M4$resid ~ Pareulo$MAP) 	# Looks okay
plot(M4$resid ~ Pareulo$MAT)	# Looks okay
plot(M4$resid ~ Pareulo$JJAMAP)	# Looks okay
par(op)

# Look at the partial plots....
avPlots(M4)

# So can infer that summer ppt is key driving variable interacting with temperature that controls 
# the abundance of C3 grassland plants in the USA. The model accounts for 37% of the data variation (P<0.0001)


# ****************************************************
# PART THREE: Model selection / averaging            *
# ****************************************************
# We did some model simplification / selection above in multiplicative example but there is a lot of debate
# concerning this, especially in respect to using p values and dropping non-significant terms
# We did it manually using a backward step function but you can do it automatically using step commands which work in a similar manner
# READ (essential that you do!!!) Whittingham for a big and valid critique of this approach:
# Whittingham, M. J., & FRECKLETON, R. P. (2006). Why do we still use stepwise modelling in ecology and behaviour?
# Journal of Animal Ecology, 75(5), 1182–1189. doi:10.1111/j.1365-2656.2006.01141.x
# The alternative is to use model selection on the basis of something else:
# Model averaging and AIC (Akaike's information criterion)


# Let's start by using the lyon.csv data and using an automated step function disregarding Whittingham et al.
# for the time being...
# Load data if you haven't already done so (loyn.csv)
Loyn <- read.csv(file.choose())

 # We'll use the model we were happy with after the analyses above. Run it again:
Loyn.lm <- lm(ABUND ~ log10(AREA) + YR.ISOL + log10(DIST) + log10(LDIST) + 
	GRAZE + ALT, data = Loyn)
# Look at the output  - you'll see some variables are not significant influences on Y (our response)
summary(Loyn.lm)

# for simplicity we won't include interactions. But even with 6 variables the number of competing models is 6 factorial!
# HUGE e.g.
# log10(AREA)
# YR.ISOL
# log10(DIST)
# log10(LDIST)
# GRAZE
# ALT
# log10(AREA) + YR.ISOL
# log10(AREA) + log10(DIST)
# log10(AREA) + log10(LDIST)
# ...
# log10(AREA) + YR.ISOL + GRAZE
# log10(AREA) + YR.ISOL + ALT
# ...
# Multiple options.......

# the step function flips explanatory variables in and out and uses AIC to remove them

step(Loyn.lm)

# Our final model [last one in the list] 
#is lm(formula = ABUND ~ log10(AREA) + YR.ISOL + GRAZE, data = Loyn)

# re-run the suggested model and look at it

Loyn.lm1 <- lm(ABUND ~ log10(AREA) + YR.ISOL + GRAZE, data = Loyn)
summary(Loyn.lm1) 
# see it is different to loyn.lm where we didn't apply selection
# the adjusted R-squared is slightly better and the interpretation different as YR.ISOL is now significant
# Notice though if we adopted a <2 AIC criteria our model would be: ABUND ~ log10(AREA) + YR.ISOL + GRAZE + ALT!

# Now let's try a model averaging approach using a new package MuMIn. This will run your competing models...
# you'll need to install it....

# Load the package
if(!require(MuMIn)){install.packages("MuMIn")} # Checks to see if you need to install the package.

library(MuMIn) # New package do some reading...

options(na.action=na.fail) # set options in Base R concerning missing values

Loyn.lm2 <- model.avg(dredge(Loyn.lm, rank = "AICc")) # code introduces model.avg(), get.models and dredge functions

summary(Loyn.lm2)
# Because we have a small sample (<40 is the rule of thumb), we should use AICc, give it a try
# So we see that area and grazing are the strongest predictors of bird abundance 
# in woodlands in Victoria. 
# Because the dataset is small (<40), we use Corrected AIC (AICc)
# NOTE: if we apply the AIC delta <2 rule the top 7 models are within 2 AIC points....all are equally likely!
# We adopt occam's razor and go for the simplist model with two variables (Area and Grazing)
# We can thus use those to construct the predictive model:

Loyn.lm.final <- lm(ABUND ~ log10(AREA) + GRAZE, data = Loyn)
summary(Loyn.lm.final)
options(na.action = "na.omit") # reset base R options

# Validate
op <- par(mfrow = c(2,2))
plot(Loyn.lm.final)
par(op)
# All looks fine.....maybe a hint of a hump in the fitted v residuals
# remember to validate the explanatory factors against the residuals

# add code in here....


# Look at the partials
avPlots(Loyn.lm.final)

# Check the coefficients
coef(Loyn.lm.final)

# Predictive regression equation is:
# abund = 6.89log10AREA - 2.85GRAZE 
# This explains approximately 64% of the variation in bird abundance

# **************************************
# PART THREE: CLASS                    *
# **************************************
# 	(a) Use the ozone.data.csv datafile;
#		(b) explore the data to look at its structure, normality, linearity, heterogeneity;
#		(c) create coplots to look for interactions between the variables;
# 	(d) create a multiplicative linear model to illustrate the what factors influences ozone levels in the city;
#		(e) Use model averaging to select the most parsimonious model;
#		(f) validate the 'best' model;
#		(g) create the predictive model and generate the regression equation.

#ozone.data.csv DATAFILE CONTENTS:
# Based on Daily readings of the following air quality values for May 1, 1973 (a Tuesday)
# to September 30, 1973 in New York
# Response - ozone: Mean ozone in parts per billion from 1300 to 1500 hours at Roosevelt Island
# Explanatory - rad: Solar radiation in Langleys in the frequency band 4000–7700 Angstroms from 0800-1200 hours at Central Park
# Explanatory - wind: Average wind speed in miles per hour at 0700 and 1000 hours at LaGuardia Airport
# Explanatory - temp: Maximum daily temperature in degrees Fahrenheit at La Guardia Airport
