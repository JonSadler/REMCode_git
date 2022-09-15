# ******************************************************************************
# This is the source script for WEEK 10: Heterogeneity and Mixed Modelling     *
# ******************************************************************************
# Jon Sadler March 2021 updated April 27th 2021

# Create a directory on your desktop called 'RWork' (or whatever you want) and download the data and script files from the Canvas website)

# Normal run of start up commands

# **************************
# Setting the home directory
setwd("~/Desktop/RWork")         # For Apple Macs
setwd(c:\Desktop\Rwork)          # For Windows machines

# **********************************************************************************************************************
# Alternatively you can do this in RStudio by selecting "Session" then "Set Working Directory" then "Choose directory" *
# **********************************************************************************************************************

# This session is about what you can do if your data points are NOT independent of each other.
# This is a tricky concept so we will take it slowly and work with one dataset and work through the 
# permutations of the different models.
# The data and code for this derive from Zuur et al. 2009 with limited modification by JPS.
# You've seen the data before its the RIKZ marine benthic data. 
# The data were sampled from 9 beaches (five samples per beach).
# We're interested in the relationship between species richness of benthic species and the height of
# the site from average sea level.

# load in the packages - some are new so will need installing
library(ggplot2)
library(lme4)
library(lattice)
library(ggfortify)
library(ggpubr)
library(lmerTest)
library(MuMIn)
library(nlme)

# load the data
beach <- read.csv(---------)

# look at the data 
glimpse(--------)


# force beach as a new factor variable called fbeach
beach$fbeach <-as.factor(beach$Beach)

# We're are interested in Richness (the response) and NAP (explanatory) in the first instance

# Draw a picture (scatterplot) and stick a linear model line through it...you've done this before
plot(Richness ~ NAP, data = beach, 
	xlab = "NAP", ylab = "Species richness", 
	 pch = 19, col = fbeach)
# run a linear model and stick a line through it...
beach.lm <- lm(Richness ~ NAP, data = beach)
abline(beach.lm)

# The linear model isn't a great fit because in part the data are nested
# let's examine how this might influence the model by faceting the data by beach...
require(lattice)
# create lattice plot
xyplot(Richness ~ NAP | fbeach, data = beach,
       panel = function(x, y, ...) {
                 panel.xyplot(x, y, ..., col = 4,pch = 19)		# add the points
                 panel.lmline(x, y, ..., col = 4, lwd = 2)		# add regression lines for each beach....
               })
# You can see that the slopes and intercepts on each beach differ!
# We deduce from this that each site on the same beach are more related to each other 
# than to sites on other beaches.
# NOTE: linear regression assumes the are the same!

# It will mean the residuals are a mess - check by plotting them...
op <- par(mfrow = c(2,2))
plot(beach.lm)
par(op)
# YUK!

#repeat using ggplot
ggplot(beach,aes(x=NAP, Richness)) + geom_point() + geom_smooth(aes(x=NAP,y=Richness), method = lm, se=FALSE) + 
  facet_wrap(~fbeach) + coord_cartesian(ylim = c(0, 20))

# We could try to add a covariate in for beach as a form of ANCOVA
beach.lm1 <- lm(Richness ~ NAP * fbeach, data = beach)

# look at results
summary(beach.lm1)
# Factors for each beach are significant but not all of them
# Note the first factor is used a baseline (remember our ANCOVA example)
# The price paid for this term is 8 regression parameters (or dfs)
# Add the interaction you have 19 parameters - Ouch.
# We've only got 45 data points!
# NOTE we also have some interaction effects

# If we extract the betas then we can see lots of different slopes
Beta<-vector(length = 9)   # nine because there are nine beaches so nine slopes
for (i in 1:9){			# create a little loop
 tmpout <- summary(lm(Richness ~ NAP, subset = (fbeach == i), data = beach)) # subset by beach
 Beta[i] <- tmpout$coefficients[2,1] # strip out the coefficients
}

# Look at the slopes
Beta
# They are quite different!!!

# Plot residual - it hasn't improved that either!
op <- par(mfrow = c(2,2))
plot(beach.lm1)
par(op)
# not much better.....

# so let's try a random effects model - random intercept model
# this will allow the intercepts for each beach to vary
# and it only costs one df!

# we need a new library to run this (it'll probably need installing)
# it is called nlme
require(nlme)
beach.lme1 <- lme(Richness ~ NAP, random = ~ 1 | fbeach, data = beach)
summary(beach.lme1)

# To understand what it means it is best to plot the fitted values
# But which ones...? The main population one: 6.55 - 2.56 x NAP? or the beaches or both?
# We opt for both

F0 <- fitted(beach.lme1,level=0) # population slope
F1 <- fitted(beach.lme1,level=1) # within beaches slopes
I <- order(beach$NAP)
NAPs <- sort(beach$NAP)
plot(NAPs, F0[I], lwd=4, col = 4, type = "l", ylim = c(0,22), # plot population line
     ylab = "Richness", xlab = "NAP")
for (i in 1:9){
   x1 <- beach$NAP[beach$fbeach==i]
   y1 <- F1[beach$fbeach==i]
   K <- order(x1)				# order and sort to avoid spaghetti plot!
   lines(sort(x1), y1[K])}		# plot beach lines
text(beach$NAP, beach$Richness, beach$Beach,cex = 0.9)

# We can also add random slope to the model. BUT it is worth asking why we might want to.
# The answer to that is that we slopes differ as well as intercepts. 
# Recall our lattice plot and the interaction effects of slope and NAP in the ANCOVA

# create random intercept and slope model
beach.lme2 <- lme(Richness ~ NAP, random = ~ 1 + NAP | fbeach, data = beach) 
summary(beach.lme2)

# plot the graph....
F0 <- fitted(beach.lme2,level=0) # population slope
F1 <- fitted(beach.lme2,level=1) # within beaches slopes
I <- order(beach$NAP)
NAPs <- sort(beach$NAP)
plot(NAPs, F0[I], lwd=4, col = 4, type = "l", ylim = c(0,22), # plot population line
     ylab = "Richness", xlab = "NAP")
for (i in 1:9){
   x1 <- beach$NAP[beach$fbeach==i]
   y1 <- F1[beach$fbeach==i]
   K <- order(x1)				# order and sort to avoid spaghetti plot!
   lines(sort(x1), y1[K])}		# plot beach lines
text(beach$NAP, beach$Richness, beach$Beach,cex = 0.9)

# As an aside we can create this plot more easily in ggplot. It is different because we are using a basic lm to fit the lines
ggplot(beach,aes(x=NAP, Richness,col=fbeach)) + geom_point() + 
  geom_smooth(aes(x=NAP,y=Richness), method = lm,se=FALSE)

# BUT WHICH MODEL IS BEST?
AIC(beach.lm, beach.lm1) 			# compare linear models
AIC(beach.lme1, beach.lme2)			# compare mixed models

# ANCOVA model appears the best but remember the residuals were a mess.
# So we're left with the random intercept and slope model.
# We need to validate that - we'll come to that in a while....

# So far we’ve looked at models with one explanatory…what about when there are more….? 
# A protocol from Zuur et al. 2009 (121-122):
# 1. Start with a model where the fixed component has all the explanatory variables - the so called beyond optimal model;
# 2. Using the beyond optimal model, find the optimal random structure and compare restricted maximum likelihood estimation (REML) 
# 	 - it’s the default on out lmd models;
# 3. Once this is done, find the optimal fixed structure. 
# 	 To compare these models (as they all have the same random structure we use maximum likelihood (ML);
# 4. Present the final model using REML estimation.

# create a factor for the exposure variable with two levels not three:
# The exposure variable gives an indication of the level of exposure the sites are subjected to at each beach.

beach$fExp<-beach$Exposure
beach$fExp[beach$fExp ==8 ] <- 10
beach$fExp<-factor(beach$fExp, levels= c(10, 11))

# STEP ONE: the best random structure
M1 <- gls(Richness ~ 1 + NAP * fExp,				# linear model 
            method = "REML", data = beach)			
# NOTE: we are using GLS here because we need to use REML to compare the models. lm doesn't use REML            
M2 <- lme(Richness ~1 + NAP * fExp, data = beach,	# Random intercept
        random = ~1 | fbeach, method = "REML")
M3 <- lme(Richness ~ 1 + NAP * fExp,data = beach,	# Random intercept and slope
        random = ~1 + NAP | fbeach, method = "REML")
        
AIC(M1, M2, M3)
# The random intercept model is best

# STEP TWO - the best fixed structure
# We cannot use MuMIn for model selection with general linear mixed models
# so we do it the hard way....
summary(M2)

# The interaction term is on the boundary of significance 0.0419 so we chuck it...
M4 <- lme(Richness ~1 + NAP + fExp, data = beach,
        random = ~1 | fbeach, method = "REML")
summary(M4)
# Exposure is at 0.02 so okay - far enough off the boundary
# So our final model is a random intercept mixed model with two explanatory factors
# NAP and Exposure. NAP has a strong influence and exposure a weaker one.

# Or we refit using ML and use MuMIn
M5 <- lme(Richness ~1 + NAP * fExp, data = beach,
        random = ~1 | fbeach, method = "ML")

require(MuMIn)
summary(model.avg(dredge(M5), fit = TRUE, subset=TRUE))

# And not too surprisingly we get the same result but MuMIn is okay with the interaction
# sitting at 0.041! BUT as it is a 1.98 AIC improvement we can go for the simpler
# model with two terms...

# AS as bit of revision have a go at predicting and plotting NAP against Richness....

# Now let's repeat this with a glm type model
# Recall we need to select an error term - they are counts so we go for poisson.
# We need a different package 

require(lme4)
# NOTICE that the code for the random terms is slighly different in this package
# Truly infuriating I know!!!

M6 <- glmer(Richness ~ NAP * fExp + (1 | fbeach), 
	data = beach, family = "poisson")	# Random slope model
# They'll be an error message - it's okay! 

M7 <- glmer(Richness ~1 + NAP * fExp + (NAP | fbeach), 
	data = beach, family = "poisson") # Random intercept and slope

# Compare models
AIC(M6,M7)

# The Anova test confirms the difference is significant
anova(M6, M7, test = "Chisq")

# Now determine the best fixed structure.....instead of refitting using ML we tell MuMin
# that the we used REML.

options(na.action=na.fail) # set options in Base R concerning missing values
summary(model.avg(dredge(M7), fit = TRUE, subset = TRUE), method = "REML")
options(na.action = "na.omit") # reset base R options
# ignore error messages they are okay...

# MODEL VALIDATION IN GLMM

# Validation of mixed models is the same as linear models but the plot command 
# only gives you the fit v residuals
plot(M7,xlab = "Fitted values", ylab = "Pearson residuals")
# They look okay - the mixed model has sorted the heterogeneity issue


##############################################################################################
##############################################################################################
#######################  GLMMS - INCORPORATING RANDOM TERMS INTO GLMS      ###################
#######################  TO SORT OUT MESSY RESIDUALS.....STICK WITH IT...!!###################
##############################################################################################
##############################################################################################

#### DATASET INFORMATION ####

##            Personal data - Habitat use by bird communities in urban landscape            ##
##        A range of bird species (recorded as richness) across Birmingham habitats         ##
##  Bird communities recorded along singular transects during the same day at each habitat  ## 
##                         Response variable (Y) - Species richness                         ##
##     Predictor variables (X) - Distance from roads (Distance), Habitat type (Habitat)     ##
##                        Random variable (XR) - Habitats (fHabitat)                        ##

#### SET-UP ####

dframe1 <- read.csv("Habitat.csv")  
summary(dframe1)
names(dframe1)

dframe1$fHabitat <- factor(dframe1$Habitat) # treat "Habitat" as factorial (categoric)

#### GENERAL LINEAR MIXED MODELS ####

library(nlme)

## MODEL 1: FIXED FACTORS (NO RANDOM EFFECTS)

model1 <- gls(Richness ~ Distance,           # Using default GLM with fixed effect (Distance)
              method = "REML",                         # Uses a Restricted Maximum Likelihood (REML)
              na.action = na.exclude,                  # Exclude NA values 
              data = dframe1)                          # Take data from dframe1 

sresid <- resid(model1, type = "normalized") # Generate the standardised residuals
hist(sresid)                                 # Check the normality of the SRs

plot(model1)                                 # Check for homoscedasticity...
plot(sresid ~ dframe1$Distance)              # SR vs. indpendent variable(s)

# Things to report in academic publications: 
# 1. Value for each variable (indicates directionality and actual change in dependent)
# 2. P value for each variable (relevance is debatable)
# 3. Degrees of freedom (total and residual)

anova(model1)                                # Multiple methods of attaining model info
model1                                       # Useful info within each of the commands
summary(model1)                              # Use all of these methods to get required info
AIC (model1)                                 # Generates the model effectiveness in AIC

# Distance does significantly improve the performance of the intercept only model 
# Distance from the road provides a significant effect on the richness of birds

# BUT!!!!! 

# The model is extremeley poor! 
# Not normal standard residual plots 
# Extreme heteroscedasticity in the residuals (indicating poor performance at low distances)

# We need to look at alternative model structures for assessing this dataset (distance has a 
# significant effect yet the model poorly represents the data)

## MODEL 2: RANDOM EFFECTS (NO FIXED EFFECTS)

model2 <- lme(Habitat ~ 1,                   # Default GLM with no fixed effects
              random = ~1| fHabitat,         # Random effect (Habitat type) 
              method = "REML",               # Again using REML
              data = dframe1)                # Take data from dframe1

sresid <- resid(model2, type = "pearson")    # Generate standardised residuals (using pearson)
hist(sresid)                                 # Assess the normality of SRs

fitted.glmm <- fitted(model2, level=1)       # Generate the fitted values from the model
plot(sresid ~ fitted.glmm)                   # Plot the SRs vs. fitted values 
plot(model2)                                 # Check for homoscedasticity...
plot(sresid ~ dframe1$fHabitat)                # SR vs. indpendent variable(s). Yuk!

anova(model2)                                # Derive the required outputs from the methods 
model2                                       
summary(model2)
AIC (model2)                                 

# Model still not brilliant, and does not fit very well 
# The influence of Habitat type on richness, however is significant 
# Both Distance from the road and habitat type need to be included as terms in the model

## MODEL 3: RANDOM AND FIXED EFFECTS (INVARIANT SLOPE FOR LEVELS OF FHabitat) ####

model3 <- lme(Richness ~ Distance,            # Default model using a fixed effect (Distance)
              random = ~1| fHabitat,          # Random effect (Habitat type)
              method = "REML",                # Using REML method
              data = dframe1)                 # Take data from dframe1

sresid <- resid(model3, type = "normalized")  # Produce the standardised residuals
hist(sresid)                                  # Check for normality using a histogram

fitted.glmm <- fitted(model3, level=1)        # Generate the fitted values from the model
plot(sresid ~ fitted.glmm)                    # Plot the SRs vs. fitted values
plot(model3)                                  # Check for homoscedasticity...

plot(sresid ~ dframe1$Distance)               # SR vs. indpendent variable(s)

anova(model3)                                 # Derive the required outputs from the methods
model3
summary(model3)
AIC(model3)

# The model fits much better but there is still some remaining patterning in the residuals
# A final change in the way that the model is structured may sort this patterning 

## MODEL 4: RANDOM INTERCEPT + SLOPE (VARIABLE SLOPE FOR LEVELS OF FHabitat) ####

model4 <- lme(Richness ~  Distance,
              random = ~ 1+Distance | fHabitat, 
              method = "REML",
              data = dframe1)

sresid <- resid(model4, type = "normalized")  # Produce the standardised residuals
hist(sresid)                                  # Check for normality using a histogram

fitted.glmm <- fitted(model4, level=1)        # Generate the fitted values from the model
plot(sresid ~ fitted.glmm)                    # Plot the SRs vs. fitted values 
plot(model4)                                  # Check for homoscedasticity...

plot(sresid ~ dframe1$Distance)               # SR vs. indpendent variable(s)

anova(model4)                                 # Derive the required outputs from the methods
model4
summary(model4)
AIC(model4)

## R-SQUARED VALUES FOR GLMMS ####

# install.packages("MuMIn") if not present
library(MuMIn)
r.squaredGLMM(model4)
# R2m = marginal R-squared (does not account for the random effects)
# R2c = conditional R-squared (accounts for the random and fixed effects)

## MODEL COMPARISON ####

AIC (model1, model3, model4) # Compares the AIC for similar model structures 
# Model 4 maintains the lowest AIC (the best performing model)
# Note that Model 2 is not directly comparable as it has a different structure (no fixed)

anova(model1, model3)  # Model 3 is significantly better than model 1
anova(model1, model4)  # Model 4 is significantly better than model 1

anova (model3, model4) # Model 4 is significantly better than model 3

# In this instance we would preferentially use Model 4 (lower AIC and greater significance)
# The model which provides the best explanatory power/performance (AIC) should always be used

#### GENERALISED LINEAR MIXED MODELS ####

# This section utilises a range of model error families and link functions to describe the data
# Altering the error family and link function can increase or decrease the model performance
# Error families should be selected based on the data structure

# Options for families and link functions include:
#   1. Gaussian (link = identity)
#   2. Inverse-gaussian (link = 1/mu^2, )
#   3. Poisson (link = log, identity, sqrt)
#   4. Quasi-poisson (link = log)
#   5. Binomial (link = logit, cloglog, probit)
#   6. Quasi-binomial (link = logit)
#   7. Gamma (link = inverse, identity, log)
#   8. Quasi (link = user-defined)  

?family() 
# For more information on family and/or link functions

library(lme4)

## MODEL 5: RANDOM INTERCEPT, DEFAULT GAUSSIAN (INVARIANT SLOPE FOR LEVELS OF FHabitat) ####

model5 <- lmer(Richness ~ Distance +  # Default model using a fixed effect (distance)
                 (1|fHabitat),        # Random effect (Habitat type)
               data = dframe1)

sresid <- resid(model5, type = "pearson")     # Produce the standardised residuals
hist(sresid)                                  # Check for normality using a histogram

fitted.glmm <- fitted(model5, level=1)        # Generate the fitted values from the model
plot(sresid ~ fitted.glmm)                    # Plot the SRs vs. fitted values 
plot(model5)                                  # Check for homoscedasticity...

plot(sresid ~ dframe1$Distance)               # SR vs. indpendent variable(s)

anova(model5)
model5
summary(model5)
AIC(model5)

### MODEL 6: RANDOM INTERCEPT, POISSON (INVARIANT SLOPE FOR LEVELS OF FHabitat) ####

model6 <- glmer(Richness ~ Distance +
                  (1|fHabitat),
                family = poisson (link = "log"), 
                data = dframe1)

sresid <- resid(model5, type = "pearson")     # Produce the standardised residuals
hist(sresid)                                  # Check for normality using a histogram

fitted.glmm <- fitted(model5, level=1)        # Generate the fitted values from the model
plot(sresid ~ fitted.glmm)                    # Plot the SRs vs. fitted values 
plot(model5)                                  # Check for homoscedasticity...

plot(sresid ~ dframe1$Distance)               # SR vs. indpendent variable(s)

anova(model6)
model6
summary(model6)
AIC(model6)

# This model fits the data much better (provides a log transformation in the link function)
# Still mild patterning in the residual plots
# On the whole a much better model

## MODEL 7: RANDOM INTERCEPT, POISSON (VARIABLE SLOPE FOR LEVELS OF FHabitat) ####

model7 <- glmer(Richness ~ Distance + 
                  (Distance|fHabitat),
                family = poisson (link = "log"), 
                data = dframe1)

sresid <- resid(model7, type = "pearson")  # Produces the standardised residuals
hist(sresid)

fitted.glmm <- fitted(model7, level=1)     # Extract the fitted (predicted) values
plot(sresid ~ fitted.glmm)                 # Check for homoscedasticity 
plot(sresid ~ dframe1$Distance)              # SR vs. indpendent variable(s)

anova(model7)
model7
summary(model7)  
AIC (model7)

# Comparison of model performance (only comparing Poisson error family)
AIC(model6, model7)
anova(model6, model7, test="Chi")

##############################################################################################
##############################################################################################
########################################### THE END ##########################################
##############################################################################################
##############################################################################################


