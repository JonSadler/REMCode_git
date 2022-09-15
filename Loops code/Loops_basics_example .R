#**********************************************************************
# PART 4: Basic programming structures - LOOPS                        *
# *********************************************************************

# for many coding applications you may need to iteratively complete a task, piping the outputs into
# dataframes for further analysis or exploration. Although R is a vector based system so loops are not always
# required or desired (due to longer processing times), loops are an essential means of automating some processes/tasks. 
# They also make for short and tidier code
# # R has three basic loop structures (repeat, while and for).
# I'll provide you with a few examples to get you started with each and develop them using specific tasks.

# repeat loops in the form of repeat <EXPRESSION>
i <- 5
repeat {if (i > 40) break else print(i); i <- i + 5;} # } indicates the loop end and return to the for statement

# while loops; in the form of while (condition) <EXPRESSION>
i <- 5
while  (i <= 40) {print(i); i <- i + 5;}

# for loops; which iterate through a vector e.g. for (var in list) <EXPRESSION>
i <- 5
for (i in seq(from=5, to=40, by=5)) print(i) 

# Notice that all these loops do the same thing print multiples of 5 upto 40. 
# Notice also that you have to instruct R to print the outcomes or it won't! Clearly 
# you can push the output into vectors and dataframes for storage if need be....

# If you are familiar with programming in Python, C, Java and even fortran you'll be underwhelmed
# by the looping structures available in R, especially the lack of iterative and foreach loops.
# But as always someone has written a package to support this:
# package: foreach

install.packages("foreach")
library(foreach)
sqrts.1to5 <- foreach(i=1:5) %do% sqrt(i)
sqrts.1to5 # look at the list

# you need the %do% operator but for hugely intensive tasks %dopar% will allow you use parallel processing.
# VERY, VERY cool.

#****************
# IMPORTANT NOTE*
#****************

# R is not really designed for looping so intensely computational loops can take a long time to process
# SO BE CAREFUL

# Some examples of using loops. One today to simulate song sparrow population growth.
# And one next week to one for produce multiple plots from a dataset (see week 3)
# Nick uses loops extensively in his code....you'll learn much more about them in his workshops
# SO DON'T PANIC if you cannot grasp them right now.


# ********MODELLING SIMULATED POPULATION GROWTH*********
# We're going to use the song sparrow data from the primer package. So install load it if you haven't already done so.
# This is count data on song sparrows in Darrtown, Ohio, USA
# code from: 
#*******************************************************
install.packages("primer")
library(primer)
data(sparrows) # data are provide in the package so we use the data() function to load it.
head(sparrows) #Look at it....
# Quick plot to look at changes over time
plot(Count ~ Year, type = "b", data = sparrows)

# create a single simulation. 
# The plan here is to create simulations of populations using the sparrow count data
# NOTE: you don't need to understand the code just look at the loops!
# We'll use the following equation Rt = Ni+1/Nt to provide annual growth figures
obs.R <- sparrows$Count[-1]/sparrows$Count[-length(sparrows$Count)]
obs.R

# Then decide the number of years to run the simulation over = 50
years <- 50

# Draw 50 R at random with replacement i.e. set the simulation object.
set.seed(3)
sim.Rs <- sample(x = obs.R, size = years, replace  = TRUE)

# create vector to receive data = the number of Rs + 1
output <- numeric(years + 1) # creates a vector 51 in length

# start projection with the count we had on the last year
output[1] <- sparrows$Count[sparrows$Year == max(sparrows$Year)]

# create for loop. For each year t, we multiple Nt by the random selection for Rt to get Nt+1
for (t in 1:years) output[t +1] <- {
  output[t] * sim.Rs[t]
}

# plot it
plot(0:years, output, type = "l")

# repeat the simulation multiple times n=10
# Need 50 x10 randomly drawn Rs in a matrix. LOOK AT MATRICES IN R
# Use sapply to generate 10 simulations

sims = 10
sim.RM <-  matrix(sample(obs.R, sims * years, replace=TRUE),
                  nrow=years, ncol=sims)

# set up simulation - we'll use output multiple times with sapply()
# then apply for loop as many times as we need, for each value of 1:sims
output[1] <- Count[Year==max(Year)]
outmat <- sapply(1:sims, function(i)  {  
  for( t in 1:years ) output[t+1] <- output[t] *  sim.RM[t,i] 
  output} )

# plot the 10 simulations.....
matplot(0:years, outmat, type="l", log="y")

