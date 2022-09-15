# ************************************************************************************
# This is the source script for WEEK 5: Linear Regression+ model validation + ANCOVA *
# ************************************************************************************
# Jon Sadler Feb 2014 updated Feb 2021

# Create a directory on your desktop called 'RWork' (or whatever you want) and download the data and script files from the Canvas website)


# **************************
# Set the home directory

# **********************************************************************************************************************
# Alternatively you can do this in RStudio by selecting "Session" then "Set Working Directory" then "Choose directory" *
# **********************************************************************************************************************

# So this is basically ANOVA but using a line so all the same assumptions hold....
# But additionally we need to know if the data are linearly structured...to do this well make a picture


# Load compensation.csv datafile from Beckerman and Petchey's book.....see readings
Growth <- read.csv(file.choose())

# Look at the file
Growth
# and its structure
str(Growth)

# read in grazing as a character variable. Better used as a factor
Growth$Grazing <- as.factor(Growth$Grazing)
#We can introduce a new way of looking at the data files using the glimpse function (from Tidyverse in the dplyr library)

library(tidyverse)
glimpse(-------) # Fill in the correct object name.....!!!!
glimpse(Growth)
# now let's plot it using R base plot() function; we're interested in knowing whether
# Root biomass (Root) has an influence or correlates with Fruit production (Fruit)

plot(Growth$Root, Growth$Fruit)
# Notice we're using the filename and $ operator to indicate the variables and that Y (response variable comes last)

# A better and more intuitive way to do it uses the data = argument

plot(Fruit ~ Root, data = Growth) # This fits the model specification for aov and linear models

# so we can see that the relationship looks linear! But also that it has two components.
# Let's tidy it up (or pimp it as Beckerman and Petchey would say)

# Add some axis labels and you can vary their size using the list(cex combination of arguments)
plot(Fruit ~ Root, data = Growth,
	xlab = list("Root Biomass",cex = 1.5), # add half to the size of the default font
	ylab = list("Fruit Production",cex = 0.5)) # halves the default font

# Now let's consider the points on the graph
# ?par gives you help on the graphics parameters
# ?points specifically on the points.

plot(Fruit ~ Root, data = Growth,
	xlab = list("Root Biomass",cex = 1), # reset size to default of 1
	ylab = list("Fruit Production",cex = 1), # reset size to default of 1
	cex = 2, pch = 21, bg = "grey") # the pch argument controls the type 21 = filled circle, bg = colour.

# You can see that there are two clusters of points here relating to the treatment - so let's explore that.
# It would be instructive to give them different colours to illustrate the patterns.

# First let's create an object that tags the colours using an ifelse() function = if / else

culr <- ifelse(Growth$Grazing == "Grazed", "green", "blue")

#look at it
culr

# to add the colours just substitute the object name into the colour argument
plot(Fruit ~ Root, data = Growth,
	xlab = list("Root Biomass",cex = 1), # reset size to default of 1
	ylab = list("Fruit Production",cex = 1), # reset size to default of 1
	cex = 2, pch = 21, bg = culr) # add culr object to the bg call

# we could have done it differently using the c() function....and then selecting the variable
culr <- c("green", "red")
culr[Growth$Grazing]
plot(Fruit ~ Root, data = Growth,
	xlab = list("Root Biomass",cex = 1),
	ylab = list("Fruit Production",cex = 1),
	cex = 2, pch = 21, bg = culr[Grazing]) # adds the selection to the colour!

# now let's finish the graphic by adding a legend
legend("topleft", legend = c("Grazed", "Ungrazed"),
	pch = 21, pt.bg = c("Green", "Red"), pt.cex = 2)

# Repeat in ggplot2
library(ggplot2)
ggplot(Growth, aes(x=Root, y=Fruit, colour = Grazing)) + geom_point() +
  labs(title = "", x = "Root Biomass", y = "Fruit Production")
# NOTICE HOW MUXH EASIER THIS IS TO DO!

#**************************************************
# CLASS exercise: create a high quality plot      *
# Datafile: Nelson.csv                            *
#**************************************************

# First linear regression output
# use the Nelson.csv data
# This is an experiment on 9 batches of flour beetles assessing their weight loss measued in mg
# at different humidity levels ranging from 0-93% humidity
# The experiment lasted 6 days

# Load data (you should have already done this)
Flour <- read.csv(file.choose())

# Look at it
str(Flour)
glimpse(Flour)

# Draw some pictures to assess linearity - you've plotted this so you can see it is!
plot(WEIGHTLOSS ~ HUMIDITY, data = Flour,
	xlab = "% Humidity",
	ylab = "Beetle weight loss (mg)",
	pch = 21, col = "blue", bg = "blue")

# You can also generate boxplots for variability using the car package 
# which align to the X and Y axes. Super useful
# load package car
require(car)

scatterplot(WEIGHTLOSS ~ HUMIDITY, data = Flour,
	xlab = "% Humidity",
	ylab = "Beetle Weight loss (mg)")

# The boxplot indicate normal distribution of data - it's quite symmetrical
# There is no indication of increasing spread of measured points around the green linear regression line
# So we can assume homogeneity of variance is likely....

# checking assumptions using ggplot2
#QQ-plot for normality. HINT: remember Y axis variable is WEIGHTLOSS (sample = ) and data file = Flour

ggplot(aes(sample = WEIGHTLOSS), data = Flour) + stat_qq() +
  stat_qq_line()
# You can add confidence intervals and labels if you want

# Run a linear model using the lm() function from base R
Flour.lm <- lm(WEIGHTLOSS ~ HUMIDITY, data = Flour)    # don't mix up your response and explanatory variables!

# Look at the output
summary(Flour.lm)   # refer to lecture PDF to see what the numbers mean i.e. for interpretation
anova(Flour.lm)     # lists the tests on the data of response to explanatory

#So what does this show us?
#First we see our model call usig the lm() function
#Then we have the spread of residuals lists from min (generally a -ve) to positive 
#The coefficients are listed next. The intercept estimate (~8.7) is the point that the regression line passes ths Y axis. Then we have the standard  error, a T value and a significance level Pr (note it's highly significant).
#The next line is your explanatory variable in this case HUMIDITY. The estimate this time is the slope of the line (it's a negative so is slopes down from left to right); the we have the same other elements. Note it is highly significant too.
#The next table is the ANOVA table and it shows that the covariate is highly significant using 1 df (there is only one covariate). And you have 7df of residuals. Which is 8-1.


# Let' look at the model more carefully by analysising the objects (see top left panel in RStudio)
# Or just type:
names(Flour.lm)

coef(Flour.lm) # show the intercept and slope values (or the betas)
residuals(Flour.lm)   # shows the residuals or errors for the fitted values fitted.values(Flour.lm)
# these should not show any patterns if graphed - more on this later in the session
fitted.values(Flour.lm) # shows the fitted values of y for every measured x

# recreate the plot with a regression line
# Draw some pictures to assess linearity - you've plotted this so you can see it is!
plot(WEIGHTLOSS ~ HUMIDITY, data = Flour,
	xlab = "% Humidity",
	ylab = "Beetle weight loss (mg)",
	pch = 21, col = "blue", bg = "blue")
	abline(Flour.lm) # add the line using the abline() function

# Now let's use the model to do something - i.e. prediction
# First we'll calculate the confidence intervals
confint(Flour.lm)

#Now we'll predict the mean beetle weight expected at 25%, 50%, 75% and 100% humidity levels
# These were not measured so it's a prediction using the regression equation
# weightloss - -0.053 + 8.704

predict(Flour.lm, data.frame(HUMIDITY = c(25, 50, 75, 100)),  # tells it what data you want 25% etc
	interval = "prediction", se = T) # uses a prediction interval and sets standard errors to TRUE

# To complete the analysis we predict across the dataset and create a new plot
# with regression equation, r-squared and line and CIs at 95%

# Recreate your plot (version 1)
plot(WEIGHTLOSS ~ HUMIDITY, data = Flour,
	xlab = "% Humidity",
	ylab = "Beetle weight loss (mg)",
	pch = 21, col = "grey", bg = "grey", axes = F) # We've turned off the default axes so need to recreate them
# You don't need to turn them off - the predicted values and CIs will work with default values (see below)
# Add x axis and reduce axis labels a little (i.e. the number on the axis not the axis label!)
axis(1, cex.axis = 0.8)
# Add the Y axis in a similar manner using horizontal tick labels
axis(2, las = 1)
# add the regression line from the model (Flour.lm) using abline.....
abline(Flour.lm, col="black")
# add the equation
text(98,9, "WEIGHTLOSS = -0.053HUMIDITY + 8.704", pos = 2)
# add r-square value
text(98,8.6, expression(paste(R^2 == 0.9078)), pos = 2) # add in text using the expression/paste functions
# create a sequence of 1000 number spanning the range of humidities (min to max)
x <- seq(min(Flour$HUMIDITY), max(Flour$HUMIDITY), l=1000)  # notice this is an 'l' = length. NOT a 1!!!!!!!
#for each value of x, calculate the upper and lower 95% confidence
y<-predict(Flour.lm, data.frame(HUMIDITY=x), interval="c")
#plot the upper and lower 95% confidence limits
matlines(x,y, lty=3, col="black") # This function add the CIs, lty = line type (dashed)
#put an L-shaped box to complete the axis
box(bty="l") # rather than a square which is the default

# Recreate your plot (version 2) - I actually prefer this version!
plot(WEIGHTLOSS ~ HUMIDITY, data = Flour,
     xlab = "% Humidity",
     ylab = "Beetle weight loss (mg)",
     pch = 21, col = "grey", bg = "grey") # We've left the axes on
# add the regression line from the model (Flour.lm) using abline.....
abline(Flour.lm, col="black")
# add the equation
text(98,9, "WEIGHTLOSS = -0.053HUMIDITY + 8.704", pos = 2)
# add r-square value
text(98,8.6, expression(paste(R^2 == 0.9708)), pos = 2) # add in text using the expression/paste functions
# create a sequence of 1000 number spanning the range of humidities (min to max)
x <- seq(min(Flour$HUMIDITY), max(Flour$HUMIDITY), l=1000)  # notice this is an 'l' = length. NOT a 1!!!!!!!
#for each value of x, calculate the upper and lower 95% confidence
y<-predict(Flour.lm, data.frame(HUMIDITY=x), interval="c")
#plot the upper and lower 95% confidence limits
matlines(x,y, lty=3, col="black") # This function add the CIs, lty = line type (dashed)

# ggplot....
library(ggplot2)

ggplot(Flour, aes(x = HUMIDITY, y = WEIGHTLOSS)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") # USING LM call. You can pass the model predicts through as well but this is way more simple

# add text using this function....from:
# https://susanejohnston.wordpress.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
ggplotRegression <- function (fit) {
   require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}
# NOTE: function only works for linear models without factors! And it uses the adjusted R2 (adj.r.squared)
fit1 <- lm(WEIGHTLOSS ~ HUMIDITY, data = Flour)
ggplotRegression(fit1)

# You can paste in text using annotate too:

p <- ggplot(Flour, aes(x = HUMIDITY, y = WEIGHTLOSS)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red")

p + annotate("text", x = 75, y = 9, label = "WEIGHTLOSS = -0.053HUMIDITY + 8.704") +
  annotate("text", x = 56, y = 8.6, label = "R2 = 0.9975")

# NOTE: x and y relate to the scale on the plots! Very useful. x = midpoint of text.
# Now let's look at model validation
# We're going to do this visually although you can use the tests we introduced last week and earlier
# To start we'll use simulated data to do this, based on post at:
http://stats.stackexchange.com/questions/52089/what-does-having-constant-variance-in-a-linear-regression-model-mean/52107#52107

# There is a R blogger post on this as well (not for the faint hearted):
http://www.r-bloggers.com/model-validation-interpreting-residual-plots/

set.seed(5) # use R's simulation tools
N  = 500 # 500 data points
b0 = 3 # set b0 or the intercept
b1 = .4 # set the slope or B1

s2 = 5 				# variance parameter
g1 = 1.5			# seed for heterogeneous variability
g2 = .015			# ditto

x        = runif(N, min=0, max=100) # create x
y_homo   = b0 + b1*x + rnorm(N, mean=0, sd=sqrt(s2            )) # groups points around the line
y_hetero = b0 + b1*x + rnorm(N, mean=0, sd=sqrt(exp(g1 + g2*x))) # increase variability in Y with measured X


# First we compare raw data
op <- par(mfrow = c(1, 2))     # 1 row and 2 columns and allocates it to object op
plot(y_homo ~ x)
plot(y_hetero ~ x)
# Indicate a likely problem as Y increases with measured X on the right hand plot
par(op) # sets graphics back to default without shutting the graphics device (unlike dev.off())

# Now let's track that through to the models. Create the linear models:
mod.homo   = lm(y_homo~x)
mod.hetero = lm(y_hetero~x)

#look at the plots
op <- par(mfrow = c(1, 2))     # 1 row and 2 columns
plot(mod.homo$resid ~ mod.homo$fitted.values, main = "Homoscedastic")
plot(mod.hetero$resid ~ mod.hetero$fitted.values, main = "Heteroscedastic")
par(op)
# The we can clearly see a wedge shape - left to right on the right hand plot

# And the same with a Scale-Location plot
op <- par(mfrow = c(1, 2))     # 1 row and 2 columns
plot(rstandard(mod.homo) ~ mod.homo$fitted.values, main = "Homoscedastic")
plot(rstandard(mod.hetero) ~ mod.hetero$fitted.values, main = "Heteroscedastic")
par(op)

# Let's repeat this with a field dataset......
# Load a new dataset mussel.csv
# data are derived from Peake and Quinn (1993) and analyse in Quinn and Keough 2002 and Logan 2010
# The study investigated abundance-area effects for invertebrates living in mussel beds in intertidal areas
# 25 mussel beds
# respone = number of invertebrates (INDIV)
# Explanatory = the area of each clump (AREA)
# additional possible response - Species richness of invertebrates (SPECIES)
# Logan models abundance but we're going to look at species richness

# load data file
Mussel <- read.csv(file.choose())

# Look at it
str(Mussel)
head(Mussel)

scatterplot(SPECIES ~ AREA, data = Mussel)
# This indicates that the data are not normally distributed (especially AREA)
# The species richness data don't look too good either. Peaked in the middle.

# Let's fit a linear model nonetheless
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
# Cook distance / leverage looks okay - no massive outliers (ie. cooks distances >1). But is it very clumped

# FINAL VALIDATION TASK - residuals against explanatory variable
plot(mussel.lm$resid ~ Mussel$AREA,
	xlab = "Mussel bed Area",
	ylab = "Residuals")
# This indicates a few large values and a slight wedge due to numerous small patches

# So what do we do?
# We can linearise the variables by transforming them and re-run the model
# I am not a fan of this - we'll look at other approaches next week!

mussel.lm1 <- lm(SPECIES ~ log10(AREA), data = Mussel)
# notice I chose not to log the response as it looked okay in the qqplot
# check the results
summary(mussel.lm1)  # look at the differences between this and unvalidated model in terms of R-square etc

# validate the model
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images...
plot(mussel.lm1)
par(op)
# These look okay…..so we accept the model and should tabulate the results
library(ggfortify)
autoplot(mussel.lm1) # same validation plot....

# FINAL VALIDATION TASK - residuals against explanatory variable
plot(mussel.lm1$resid ~ log10(Mussel$AREA),
	xlab = "Log10 of Area",
	ylab = "Model residuals")

# And create the plot....some homework for you..! Quicker to use ggplot - see code above!

# An alternative model would be to fit a second order polynomial to the area variable
mussel.lm2 <- lm(SPECIES ~ AREA + I(AREA^2), data = Mussel) 		# I(AREA^2) fits the 2nd order polynomial

# check the results....
summary(mussel.lm2)

# validate the model
op <- par(mfrow = c(2, 2))  # this gives us a 2 x 2 panel with four images...
plot(mussel.lm2)
# Not as good as the log10 model in terms of these plots
# However, area effects are normally better modelled using a power model in the form of y = x^be
# We'll revisit this next week
# set graphics to default
par(op)

# The are SIX in built validation plots
# You can call these individually using the 'which' argument within plot(my model object name)
# So here they are all six in one plot
op <- par(mfrow = c(2, 3))  # this gives us a 2 x 3 panel with six images...
plot(mussel.lm1, which = 1) 	# residual v fitted
plot(mussel.lm1, which = 2) 	# Normal Q-Qplot
plot(mussel.lm1, which = 3)		# Scale-Location plot
plot(mussel.lm1, which = 4)		# Cook's Distance by observation (sample number). Some texts state D of >1 are an issue but we need to be more conservative
# so we can use 4/(N−k−1), where N is the number of observations and k the number of explanatory variables
plot(mussel.lm1, which = 5)		# Residuals v Leverage (with Cook's contours)
plot(mussel.lm1, which = 6)		# Cooks v leverage (not easy to interpret so seldom used)
# set graphics to default
par(op)

# what cook values are okay? using 4/(N−(k−1))
4/length(Mussel$INDIV) # K-1 in this case is 1-1 = 0 (as we only have one explanatory variable)
# 0.16 is threshold so observation is 21 might be an issue....I'd be tempted to leave it in. Not a huge issue.

# The car package has clever stuff too....
require(car)
# Influential points....leverage etc # Influence Plot
influencePlot(mussel.lm1, id.method="noteworthy", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# again 21 (and 3) are worth looking at...but not huge enough for panic measures

# You can also look at the Normality of Residuals
# qq plot for studentized resid
qqPlot(mussel.lm1, main="QQ Plot")
# looks fine...

#************************************************
# PART THREE: ANCOVA (MORE THAN ONE EXPLANATORY)*
#************************************************

# Load the limpet.csv file (code from Beckerman and Petchey 2012)
# You should know the code by now!!!
# And look at its structure

#************************
# Do some plotting....
# CLASS EXERCISE: you need a boxplot and scatterplot (5 Mins)
#************************
Limpet <- read.csv(file.choose()) 
glimpse(Limpet)
# make season a factor
Limpet$SEASON <- as.factor(Limpet$SEASON)
# check it worked
glimpse(Limpet)


boxplot(EGGS ~ DENSITY, data = Limpet)
boxplot(EGGS ~ SEASON, data = Limpet)

# Or using ggplot. NOTE: recall density is an integer so to make it boxplot in 
# ggplot we need to force it to a factor using as.factor(). Base graphics do not need this...see above.
ggplot(Limpet, aes(x=as.factor(DENSITY), y=EGGS)) + geom_boxplot() + theme_bw() + labs(title ="", x = "Density treatment", y = "Number of Eggs")
ggplot(Limpet, aes(x=SEASON, y=EGGS)) + geom_boxplot() + theme_bw() + labs(title ="", x = "Season", y = "Number of Eggs")

#adding theme_bw coerces ggplot to plot in black and white. 
# Try removing that chunk of code to see the default colour version.

# Plot the lines and points one plot to compare them - we are now moving to use ggplot

ggplot(Limpet, aes(x= DENSITY, y = EGGS, colour = SEASON)) + geom_point() + 
 theme_bw()

# now let's run a linear ANCOVA model using the function lm()
# REFER TO LECTURE MATERIAL TO LOOK AT THE HYPOTHESES WE CAN TEST USING THIS APPROACH...IMPORTANT
Limpet.lm <- lm(EGGS ~ DENSITY * SEASON, data = Limpet) # We going for the interaction model....

# so the code includes EGGS ~ DENSITY * SEASON
# * = interaction code.....

# First we need to evaluate the assumptions
op <- par(mfrow = c(2,2))
plot(Limpet.lm)
par(op)
# They look pretty good to me. Slight indication of a step up in residual v fitted values but nothing to
# worry about

# Once we are happy the model is good look at the results using two commands anova() and summary()
anova(Limpet.lm)
summary(Limpet.lm)

# REFER TO THE LECTURE FOR A DETAILED INTERPRETATION OF THESE RESULTS. THIS IS AN IMPORTANT STEP
# DON'T IGNORE IT

# Plot figure and finalise the analysis
# Remember we have two separate lines here so we need to distinguish between them:
# You can select them using the [] selection brackets
Limpet.lm$coeff[1] # regression coefficient for spring intercept
Limpet.lm$coeff[2] # regression coefficient for spring slope
Limpet.lm$coeff[3] # regression coefficient for the difference between the spring and summer intercepts
Limpet.lm$coeff[4] # regression coefficient for the difference between the spring and summer slopes

# plot
plot(EGGS ~ DENSITY, data = Limpet, pch = 19,cex = 1.5,
	col = c("Black", "Red")[Limpet$SEASON],
	xlab = list("Density", cex = 1.2),
	ylab = list("Eggs Produced", cex = 1.2))
# Now add a legend
legend(35, 3, legend = c("Spring", "Summer"),
	col = c("black", "red"), pch = 19)
# Now add the coefficients from above to the abline command to get both lines
abline(coef(Limpet.lm)[1], coef(Limpet.lm)[2]) 	# This adds the spring regression line (default col = black)
# Now add the summer regression line
abline(coef(Limpet.lm)[1] + coef(Limpet.lm)[3],	# Summer intercept
	coef(Limpet.lm)[2] + coef(Limpet.lm)[4],	# Summer slope
	col = "red")

# We also can recreate the plot using the predict() function you were introduced to earlier in this session
# Let's that. NOTE: this is an experiment so we have replicates (3 replicates of 4 densities and 2 season)
# or 3 x 4 x 2 = 24
# We can see this if we use the regression equation to predict 'y' at each 'x' (which we measured)
predict(Limpet.lm) # have a look - the predictions are repeated in groups of 3s!!

# So we need a slightly different approach to predicting
# First we need to create a vector pulling through the replicates that are repeated (DENSITY and SEASON)

new.x <- expand.grid(DENSITY = unique(Limpet$DENSITY))  # expand.grid is an new function but you've encountered unique() before

# look at it
new.x

# Now let's add in SEASON
new.x <- expand.grid(DENSITY = unique(Limpet$DENSITY), # levels is a new function too! Look it up
	SEASON = levels(Limpet$SEASON))

# look at it
new.x

# Now let's predict the values
predict(Limpet.lm, newdata = new.x) # give eight values that are the predictions for y at the 8 elements we created in new.x

# now let's give them a name (name the object)
# and add them to a dataframe ahead of plotting (same as last - compare the code)
limpet.predictions <- predict(Limpet.lm, newdata = new.x)
preds.for.plot <- data.frame(new.x, limpet.predictions) # add to a new data frame
#Look at it
preds.for.plot

# You're now ready to complete the plot....
plot(EGGS ~ DENSITY, data = Limpet, pch = 19,cex = 1.5,
	col = c("Black", "Red")[Limpet$SEASON],
	xlab = list("Density", cex = 1.2),
	ylab = list("Eggs Produced", cex = 1.2))
	xlim = c(0, 50) # limit the X axis
# Now add a legend
legend(35, 3, legend = c("Spring", "Summer"),
	col = c("black", "red"), pch = 19)

# add lines using the preds.for.plot data frame. This is ANCOVA so we're not worried about the CIs !!!
lines(limpet.predictions ~ DENSITY, subset(preds.for.plot,
	SEASON == "spring"))
lines(limpet.predictions ~ DENSITY, subset(preds.for.plot,
	SEASON == "summer"), col = "red")

# You may be wondering why we are bothering with predicting when we can achieve the same
# plot using albine commands???!!!

# Well - it allows us to predict and other useful information
# We can modify the code to get the predicted number of eggs for each season, which aids our interpretation

new.x <- expand.grid(DENSITY = mean(Limpet$DENSITY), # we are looking for a mean...
	SEASON = levels(Limpet$SEASON))
# look at it
new.x

# now predict the mean values per season
predictions.mean <- predict(Limpet.lm, newdata = new.x)
preds.for.plot <- data.frame(new.x, predictions.mean)

# Look at it - it gives you the mean density and mean eggs production
preds.for.plot

# Now predict for different levels
new.x1 <- expand.grid(DENSITY = c(5, 10, 25, 35),
	SEASON = levels(Limpet$SEASON))
# look at it
new.x1

# now predict the mean values per season
predictions.levels <- predict(Limpet.lm, newdata = new.x1)
preds.for.plot1 <- data.frame(new.x1, predictions.levels)

# Look at it - it gives you the mean density and mean eggs production
preds.for.plot1

# As always the plot can be generated in ggplot

#Make some new density values to predict at and drop into a dataframe. expand.grid() is a function that generates a grid of numbers—essentially it builds a factorial representation of any variables you provide to it, and returns a data frame. 

new.x <- expand.grid(DENSITY = seq(from = 8, to = 45, length.out = 10))

#look at it

head(new.x)

#Now let's add SEASON in. Recall it has two levels - spring and summer.


new.x <- expand.grid(
  DENSITY = seq(from = 8, to = 45, length.out = 10),
  SEASON = c("spring","summer")) 

#look at it (again)
head(new.x) 

#We will use predict() with three arguments: a model, a value for newdata, and a request for confidence intervals. And we assign what predict returns to an object called new.y:

new.y <- predict(Limpet.lm, newdata = new.x,
                 interval = 'confidence')

#Check it...

head(new.y)

# We have new.x, which looks like a version of the explanatory variables. And we have new.y, which is estimated using the coefficients from the model we have actually fitted, along with the 95% confidence interval around each value, with nice names — fit, lwr, and upr
# The next step Beckerman et al. call ‘housekeeping’; an important part of using R, and at this point in the plotting cycle. To do this we combine the new y’s with the new x’s, so that we have a clear picture of what it is you’ve made. We can do that with the function data.frame(). We call the data frame Limpet.addition,’cause we are gonna add these to the plot.

Limpet.addition <- data.frame(new.x, new.y)
Limpet.addition <- rename(Limpet.addition, EGGS = fit)

#Check it

head(Limpet.addition)

# So, now you have a new, small data frame that contains the grid of seasons and densities, as well as the predicted values and 95% CI at each of these combinations. You did not need to specify the coefficients, or the equations for the lines. predict() does all of that for you. You did not need to add and subtract 1.96*standard error to and from each value either to generate the CI… predict() does all of that for you.
#You are now ready to plot the and use the predictions

ggplot(Limpet, aes(x = DENSITY, y = EGGS, colour = SEASON)) + 
  # first add in the points
  geom_point(size = 5) +
  # now add in the fits and CIs
  # NOte DENSITY and EGG are inherited so don't need specifying
geom_smooth(data = Limpet.addition, aes(ymin = lwr, ymax = upr,
                                        fill = SEASON), stat = 'identity') +
  # now adjust colours
  scale_colour_manual(values = c(spring = "green", summer = "red")) +
  scale_fill_manual(values = c(spring = "green", summer = "red")) +
  # add theme
  theme_bw()

# Note this way we get the CKs too!!

#************************************************
# PART FOUR: Class Exercises                    *
#************************************************
#1. Datafile = old faithful. It is in the base system already as a dataframe called "faithful". Type str(faithful) to see its structure.
# There are two observation variables in the dataset.
# The first one, called eruptions, is the duration of the geyser eruptions (mins).
# The second one, called waiting, is the length of waiting period until the next eruption (mins).
# Response = eruptions
# Explanatory = waiting
# 272 rows v 2 columns see dim(faithful)

# 2. Use the mussel data (mussel.csv) but with response variable as abundance (Mussel$INDIV)
# not species richness (Mussel$SPECIES)

# 3. Use the compensation.csv dataset and undertake an ANCOVA analysis

#************************************************
# TASKS - repeat on all datasets:
# 1. Create plots to assess the assumptions:
#	(a) On the raw data;
#	(b) Run the model;
#	(c) Validate it.
# 2. Create a final model plot with predicted data, 95% CIs, regression equation, R-squared value;
# 3. Interpret the model output.
#************************************************
