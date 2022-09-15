# ******************************************************************************
# This is the source script for WEEK 7: When things are not linear             *
# ******************************************************************************
# Jon Sadler Mar 2021 

# Create a directory on your desktop called 'RWork' (or whatever you want) and download the data and script files from the Canvas website)
# **************************
# Setting the home directory
setwd("~/Desktop/RWork")         # For Apple Macs
setwd(c:\Desktop\Rwork)          # For Windows machines

# **********************************************************************************************************************
# Alternatively you can do this in RStudio by selecting "Session" then "Set Working Directory" then "Choose directory" *
# **********************************************************************************************************************

# *********************************
# PART ONE: POLYNOMIAL REGRESSION *
# *********************************

# load data file
Mussel <- read.csv(file.choose())

# You've used this before....
# data are derived from Peake and Quinn (1993) and analyse in Quinn and Keough 2002 and Logan 2010
# The study investigated abundance-area effects for invertebrates living in mussel beds in intertidal areas
# 25 mussel beds
# response = number of invertebrates (INDIV)
# Explanatory = the area of each clump (AREA)
# additional possible response - Species richness of invertebrates (SPECIES)
# Logan models abundance but we're going to look at species richness


# Look at it
str(------)
head(------)
glimpse(-----)

# Load car package
require(car)

scatterplotMatrix(~SPECIES + AREA, data = Mussel, diagonal = list(method = "qqplot"))
# This indicates that the data are not normally distributed (especially AREA)
# The species richness data don't look too good either. Peaked in the middle.

# Let's fit a linear model nonetheless to look at the outcome 
mussel.lm <- lm(SPECIES ~ AREA, data = Mussel)

summary(mussel.lm)

# Now check assumption by using R's inbuilt model validation plot defaults
# set graphics parameters because we want all the plots on one graphic
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images and allocates it to object op
# Plot the diagnostics
plot(mussel.lm)
par(op)    # turns graphics device back to default of one plot per page!

# Residuals v Fitted indicate a problem. It's wedge shaped and humped!
# qqplot is a bit dodgy but might is okay
# Scale-Location plot is variable
# Cook distance / leverage looks okay - no massive outliers (ie. cooks distances >1)

# FINAL VALIDATION TASK - residuals against explanatory variable
plot(mussel.lm$resid ~ Mussel$AREA,
	xlab = "Mussel bed Area", 
	ylab = "Residuals")
# This indicates a few large values and a slight wedge due to numerous small patches

# Last time we used a linear transformation on AREA to deal with the humped nature of the explanatory
# And also the nasty hetergeneous spread of the variables.

# But the hump might be real so we can also model it using a polynomial
# So let's fit a second order polynomial to the area variable
mussel.lm1 <- lm(SPECIES ~ AREA + I(AREA^2), data = Mussel) 		# I(AREA^2) fits the 2nd order polynomial

# check the results....
summary(mussel.lm1)

# validate the model
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images...
plot(mussel.lm1)
# set graphics to default
par(op)
# Mostly okay but QQ-plot not fab. Usable.

# FINAL VALIDATION TASK - residuals against explanatory variable
plot(mussel.lm1$resid ~ Mussel$AREA,
	xlab = "Mussel bed Area", 
	ylab = "Residuals")
	
# Now let's use a third order polynomial additively...to see if we get improved fit

mussel.lm2 <- lm(SPECIES ~ AREA + I(AREA^2) + I(AREA^3), data = Mussel) 		
# I(AREA^2) fits the 2nd order polynomial
# I(AREA^3) fits the 3nd order polynomial

# check the results....
summary(mussel.lm2)
# cubic term is significant to 0.05 level
# validate the model
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images...
plot(mussel.lm2)
# set graphics to default
par(op)

# FINAL VALIDATION TASK - residuals against explanatory variable
plot(mussel.lm2$resid ~ Mussel$AREA,
	xlab = "Mussel bed Area", 
	ylab = "Residuals")
	
# NOTE: you can get the same result using the following code
mussel.lm3 <- lm(SPECIES ~ poly(AREA, 3), data = Mussel) 	

# check the results....
summary(mussel.lm3)

# We now have three models which one is the best....? We can discount M0 as the diagnostics are poor
# But what about M1 and M2. Luckily we can use ANOVA to establish whether the models differ from each other and AIC to assess which model is most parimonious...

anova(mussel.lm, mussel.lm1, mussel.lm2)
AIC(mussel.lm, mussel.lm1, mussel.lm2)

# ANOVA and AIC indicate mussel.lm2 is the 'best' model

# Finalise the model and plot it....
# plot base graph (with CIs)
plot(SPECIES ~ AREA, data = Mussel,
	pch = 16, 
	xlab = (expression(paste("Mussel bed area ", "(", m^2, ")"))), 
	ylab = "Species Richness")
# predict across the data
x <- seq(min(Mussel$AREA), max(Mussel$AREA), l=1000) # 1000 steps....
y <- predict(mussel.lm2, data.frame(AREA = x), interval = "c")
matlines(x, y, lty = 1, col = 1)

# plot base graph (without CIs)
plot(SPECIES ~ AREA, data = Mussel,
	pch = 16, 
	xlab = (expression(paste("Mussel bed area ", "(", m^2, ")"))), 
	ylab = "Species Richness")
# predict across the data
x <- seq(min(Mussel$AREA), max(Mussel$AREA), l=1000) # 1000 steps....
points(x, predict(mussel.lm2, data.frame(AREA = x)), type = "l")

# Finalise the model and plot it (with CIs). Use ggplot.....
# load library for ggplot2 [your task].
# Add in correct X and Y
ggplot(Mussel,aes(x=------, y=-------)) + geom_smooth() + geom_point()


# *********************************
# PART TWO: NON-LINEAR REGRESSION *
# *********************************
# Don't worry too much if you don't get this immediately....
# This regression merely fits a line to data and doesn't tell too much about causality
# We're going to use the same data as above

# Specify the model.....starting with a power function
mussel.nls1 <- nls(SPECIES ~ a * AREA^b,
	start = list(a = 0.1, b = 1), data = Mussel) # we will use these starting numbers....
summary(mussel.nls1)

# Quick check of diagnostics. The normal plot functions won't work so we need to create the plot
plot(resid(mussel.nls1) ~ fitted(mussel.nls1))

# or we can create a self start function to do it...hang in in there.....
SSpower <- selfStart(~ a*x^b,
                     function(mCall, data, LHS)
                     {
                       xy <- sortedXyData(mCall[["x"]], LHS, data)
                       if(nrow(xy) < 3) {
                         stop("Too few distinct x values to fit a power function")
                       }
                       z <- xy[["y"]]
                       xy[["logx"]] <- log(xy[["x"]])     
                       xy[["logy"]] <- log(xy[["y"]])  
                       aux <- coef(lm(logy ~ logx, xy))
                       pars <- c(exp(aux[[1]]), aux[[2]])
                       setNames(pars,
                                mCall[c("a", "b")])
                     }, c("a", "b"))

# use it to generate the nls model
mussel.nls2 <- nls(SPECIES ~ SSpower(AREA, a, b), data = Mussel)
summary(mussel.nls2)

# Notice that it produces exactly the same results!!!

# But this kind of relationship might be better modelled using an asymptotic model
# So let's try that one
mussel.nls2 <- nls(SPECIES ~ SSasymp(AREA, a, b, c), data = Mussel) 
# notice it's a similar self-start function
# notice also we're overwriting mussel.nls2 because it provides the same outcome as nls1

summary(mussel.nls2)

# so which one fits the data better? We already know that the polynomial model is better than
# the linear one, which is a mess in terms of validation anyway but we'll compare them all
# note we cannot use anova() because the models are specified differently so not nested

AIC(mussel.lm, mussel.lm2, mussel.nls1, mussel.nls2)
AIC(mussel.lm, mussel.lm2, mussel.nls1)
# The power function appears to be the best 

# Let's plot them on one graph to show the different patterns
plot(SPECIES ~ AREA, data = Mussel,
	pch = 16, 
	xlab = (expression(paste("Mussel bed area ", "(", m^2, ")"))), 
	ylab = "Species Richness")

# predict across the data and plot all three lines
x <- seq(min(Mussel$AREA), max(Mussel$AREA), l=1000) # 1000 steps....
points(x, predict(mussel.lm2, data.frame(AREA = x)), type = "l", lty = 1) # continuous line (polynomial)
points(x, predict(mussel.nls1, data.frame(AREA = x)), type = "l",lty = 2) # large dashed line (power function)
points(x, predict(mussel.nls2, data.frame(AREA = x)), type = "l",lty = 3) # short dashed line (Asymptotic function)

# First you need to recreate the dataframe with the addition lines derived from the three models: mussel.lm2, mussel.nls1, mussel.nls2. 
# To do this we use the predict function to pull out the information from the model objects (as we did above). 
# Then pull out the Species and Area information in the dataframe and store this in individual vectors (lm2,nls1,nls2,Species, Area).
# We then create a new dataframe usng cbind.data.frame()

lm2 <- predict(mussel.lm2)
nls1 <- predict(mussel.nls1)
nls2 <- predict(mussel.nls2)
Species <- Mussel$SPECIES
Area <- Mussel$AREA
ggplot_comparison <- cbind.data.frame(Species,lm2,nls1,nls2,Area) # cbind.data.frame function creates the dataframe from the vectors

# Here's a long hand way of doing it using the columns in the dataframe we've just created above. We set up the aesthetics using x=Area and y=Species and add in the measured datapoints using geom.point. Then we add in each predicted model element as an indivudual line using geom.smooth (or geom_line()) and telling it the aes for each line (or typing the column name (e.g. lm2, nls1 and nls2)). Adding se=FALSE removes the CIs around the lines and linetype specifies the type of line.
ggplot(ggplot_comparison, aes(x = Area , y = Species)) + geom_point() + geom_smooth(aes(y = lm2), size = 1,se=FALSE, linetype = 1) + geom_smooth(aes(y = nls1), size = 1,se=FALSE, linetype = 2) + geom_smooth(aes(y = nls2), size = 1,se=FALSE,linetype = 3) + theme_bw() + 
  scale_x_continuous("Mussel bed area") + scale_y_continuous("Species Richness")

#Or we can force (or melt) the data i.e. the three columns, lm2,nls1,nls2 into one column in a melted dataframe using the gather() function from dplyr.
library(tidyr)
ggplot_long <- ggplot_comparison %>% gather(variable, value,-Species,-Area) 
# three columns are now combined in a new column called 'variable' and their values in a column called 'value'.
#Now use those columns to plot all three lines using 'value' as y, and linetype as 'variable' in the aes element of a geom.smooth(). This is neater and you get to specify a legend title using labs(linetype='TITLEMNAME'). You can vary linetypes and colours to match what you want.

ggplot(ggplot_long, aes(x = Area , y = Species)) + geom_point() + 
  geom_smooth(method="lm", formula=y~x+I(x^2)+I(x^3), se=FALSE,linetype = "solid") + 
  geom_smooth(method="nls", 
              formula=y~a*x^b, 
              method.args=list(start=c(a=0.1,b=1)) ,se=FALSE, linetype = "dashed") +
  geom_smooth(method="nls", 
              formula=y ~ SSasymp(x, a, b, c) ,se=FALSE, linetype = "dotted") +
  theme_bw() + 
  scale_x_continuous("Mussel bed area") + scale_y_continuous("Species Richness") + 
  labs(linetype='Model fits')


ggplot(ggplot_long, aes(x = Area , y = Species)) + geom_point() + 
  geom_smooth(aes(y=value, linetype = variable),se=TRUE) + theme_bw() + 
  scale_x_continuous("Mussel bed area") + scale_y_continuous("Species Richness") + 
  labs(linetype='Model fits')
# I know it's tough....but it's good 'data wrangling' revision

# let's look for a more readily understandable plan B..:)
# *****************************************
# PART THREE: GENERALISED ADDITIVE MODELS *
# *****************************************
# We're going to use the vegetation2.csv data - you've seen this before too! 

# Load the data
Veg <- read.csv(file.choose())
# Look at it

str(Veg)
head(Veg)
# Experiment to look at grassland richness over time in Yellowstone National Park
# The study Skkink et al. 2007 identified a range of important variables and we are going to use those
# 
# Response variable = SR or species richness of plants per transect
# Explanatory - Rock content (ROCK)
# Explanatory - baresoil (BARESOIL)
# Explanatory - Litter (LITTER)
# Explanatory - ppt in Autumn (FallPrec)
# Explanatory - Max Spring temperature (SprTmax)
# Explanatory - Year of transect (Time)
# Explanatory - Transect number (Transect)

# We are focusing on ROCK as the key variable highlighted in paper.....not all the variables

# Do some preliminary visualisation with scatters 
require(car) # - if you need it for a scatterplot.....
# add in code for scatterplotMatrix()
scatterplotMatrix(--------)

# It's not a linear response so let's try a GAM with these two variables
# Load the package - we're using Wood's mgcv library

require(mgcv)
Veg.gam1 <- gam(SR ~ s(ROCK), data = Veg) # We are using all the defaults here...!!!
plot(Veg.gam1) # This gives us the smoother curve not validation plots...see lecture on how to interpret this
# NOTE: the s(ROCK) term fits the smoother.

# look at the result
summary(Veg.gam1)
# Validation plots are called using:
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images...
gam.check(Veg.gam1) 
# set graphics to default
par(op)
# All looks fine...maybe slight indication in the qq-plot of a skew on the high values

# only works with the mgcv package. 
#In other packages you need to manually create the plots or use visualisation packages

# You seen them all before except plot 4 (response v Fitted)
# Everything looks okay apart from the plot 4 which should be a linear pattern!
# There are good reasons as to why it isn't but we'll address those later. We'll proceed as if it is fine.
# To get the line on a 'real' axis you need to use the predict command

# plot base graph (with CIs)
plot(SR ~ ROCK, data = Veg,
	pch = 16, 
	xlab = "% ROCK in substrate", 
	ylab = "Species Richness")
# predict across the data
x <- seq(min(Veg$ROCK), max(Veg$ROCK), l=100) # 100 steps....
y <- predict(Veg.gam1, data.frame(ROCK = x), se = TRUE)  # using standard errors se = TRUE
# add lines
lines(x, y$fit)   # plots the fitted values
lines(x, y$fit + 2 * y$se.fit, lty = 2) # plots a dashed line for 2 * SE above the fit
lines(x, y$fit - 2 * y$se.fit, lty = 2) # plots a dashed line for 2 * SE below the fit

# Repeat the plot using ggplot. Note you need to use geom_smooth() to get the lines and 
# geom_point() to display the points
ggplot(--, aes(----, -----)) + geom_smooth()b+ geom_point()+
  xlab("% Rock Substrate") + ylab("Species Richness")

# Or we can fit the formula directly by using the method function and formula.
# if you look at the gam object [Veg.gam1] 

Veg.gam1  # you can see the df estimated as 3.78 by the package.
# we add that tp bs(x,DF) element of the formula to the nearest integer (so 4!)

ggplot(Veg, aes(ROCK,SR)) + stat_smooth(method=gam, formula = y~splines::bs(x,4))+geom_point()+
  xlab("% Rock Substrate") + ylab("Species Richness")


# *********************************
# PART FOUR: CLASS EXERCISES      *
# *********************************

# PART ONE : POLYNOMIAL REGRESSION 
# 		1. Use the mytilus.csv datafile (see Logan 2010 for data and analysis) :
#		(a) explore the data to look at its structure, normality; 
#		(b) Assess its linearity (using the Car package - 'you've done this before');
#		(c) add in polynomial terms for the distance variable up to the 5th order (quintic model);
#		(d) validate the model. NOTE: 5 polynomials may overfit the model. Most biologists would have kittens over its use! What is the 		correct polynomial fit? That is compare the models.....
#		(e) create the predictive model and generate the regression equation.
# Data description: the data are Lap94 allele frequency data from mussels at increasing distances from Southport harbour
# Response is LAP
# Explanatory is DIST
#  IMPORTANT - fit a transformation to the response from the outset using
# an arcsin transformation  - arc(sqrt(LAP))*180/pi
# the transformed code is Y in your regression equation.....
# First model terms: lm(asin(sqrt(LAP))*180/pi ~ DIST)

# PART TWO :NON-LINEAR REGRESSION
# I have no exercise for you here......! My view is that GAMs are the best way to deal with data of this nature
# unless you are planning to do some physical modelling where the maths really matters.

# PART THREE: GENERALISED ADDITIVE MODELLING (GAMS)
#		1. Datafile = old faithful. It is in the base system already as a dataframe called "faithful".  
#		(a) Explore the data (use the car package scatterplot)
#		(b) Generate a linear model - you've already done this once!
# 		(c) Generate a GAM
#		(d) Compare the two models using AIC

#		2. Use the vegetation2.csv data to create a multiple factor GAM 
		# Use everything but disregard Time and Transect
#		(a) Explore the data
#		(b) Look for correlations between the variables to check for covariability
#		(c) Create a GAM
#		(d) Do some model selection (using the same approach as a linear model)
#		(e) Validate the best model

		