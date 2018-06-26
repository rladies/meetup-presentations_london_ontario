## R-Ladies #ldnont Script Writing
## Developed by Rachael Smyth

# Adding comments to your script will help you keep track of what analyses are being
# run, what decisions are being made, and why you're making them

# This script can be used to gather descriptive information about your sample,
# run an independent samples t-test,
# and create a histogram of your data

#sink("TV_Data_Descriptives_Independent_T-Test")

## Load libraries

# Load the libraries you'll need for the analyses to be run
# In this script, the packages required are all automatically loaded,
# but if you needed to load a package it would appear as follows:
library(psych)  
library(graphics)


## Load in Data

# This data presents the number of hours per week that two groups of children 
# from two different areas in London watch TV.
TV_Data <- read.csv(file = "/Users/rachael/Desktop/Masters/R data/R-Ladies/TV_Data.csv", 
                    header = TRUE)
TV_Data


## Rename Variables

# I want to make sure my labels are as descriptive as possible, so I might rename
# the variable "TV_time" to "TV_hours_week"
names(TV_Data)[names(TV_Data)=="TV_time"] <- "TV_hours_week"

# Let's look at our dataset again to see the change we made:
TV_Data


## Analyses

# Descriptives

# Calculate the mean of the data
mean(TV_Data$TV_hours_week)

# Calculate the standard deviation of the data
sd(TV_Data$TV_hours_week)

# Calculate the median of the data
median(TV_Data$TV_hours_week)

# Calculate the mode of the data
# mode used as variable name to calculate the mode of the change in sleep
mode<-table(TV_Data$TV_hours_week) 
subset(mode, mode==max(mode))

# Find the range of the data
range(TV_Data$TV_hours_week)

# See a summary of the data
summary(TV_Data)

# "describe" provides you with information about: the n size, the mean, the sd, the median, min, max, 
# range, skewness, kurtosis, and standard error.
describe(TV_Data$TV_hours_week)

# "describeBy" provides the same type of information but splits the data so that it's providing
# information by group.
describeBy(TV_Data$TV_hours_week, TV_Data$Group)

# Creating Functions

# You can also create your own functions so that they display the information that you want, but
# leave out the information you aren't interested in. Let's say you wanted to know the mean, sd, 
# and range of the # of hours watched for each group, you could create a function that
# just provides you with that information and call it "Descriptives".
Descriptives <- function(x)  #I like to keep my function names general but explanatory so I can recycle them
  c(MN = mean(x), SD = sd(x), RG = range(x))
TV_Descriptives <- with(TV_Data, 
                   aggregate(TV_hours_week ~ Group, FUN = Descriptives))
TV_Descriptives  #When I use a function, I label the output more specifically based on the data

# Performing an Independent Samples T-Test

# Now I can run a t-test to see whether there is a group difference in the number of hours of TV watched
# in a week. 
t.test(TV_hours_week ~ Group, data = TV_Data,
       paired = FALSE,
       var.equal = FALSE,
       conf.level = 0.95)

## Creating a Histogram

# Finally, I can make a graph to show the distribution of hours children spent watching TV each week.

hist(TV_Data$TV_hours_week,main = "Histogram of # of hours of TV watched/week", 
     xlab = "# of hours of TV watched per week", ylab = "# of children")


#sink()
