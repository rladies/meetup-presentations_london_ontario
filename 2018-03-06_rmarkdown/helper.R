################################################### ###
# Helper script for analyzing Starbucks drink data
################################################### ###

# R-Ladies LdnOnt Workshop on Using R Markdown to generate reports and manuscripts
# March 6, 2018
# Thea Knowles
# thea.knowles@gmail.com

# Setup ----

# Run this line if you you've never installed these packages:
#    install.packages(c("ggplot2", "dplyr", "kableExtra"))
# Note: you can also check if you have them installed, and even install them directly, in your RStudio Packages panel or by clicking Tools >> Install Packages

library(ggplot2)
library(dplyr)
library(kableExtra)

# Load data ----
starbucks <- read.csv("starbucks_drinkMenu_expanded.csv")

# View data
# View(starbucks)

# Summarise data ----
calsugs <- summarise(group_by(starbucks, Beverage_category),
                  cals = mean(Calories),
                  sug = mean(sugars))

# Models ----
# Model Calories as a function of sugar
mod1 <- lm(Calories ~ sugars, data=starbucks)
summary(mod1)

# IMPORTAT NOTE! This is just an example. We are missing some important steps here! See Jaky's intro to regression to see how you'd check model assumptions, etc.!


# Plots ----
## Calories x sugars ----
calsugs_plot <- ggplot(data = starbucks, aes(x = sugars, y = Calories)) + 
     geom_point()+
     geom_smooth(method="lm")

calsugsbevs_plot <- ggplot(data = starbucks, aes(x = sugars, y = Calories)) + 
     geom_point()+
     geom_smooth(method="lm")+
     facet_wrap(~Beverage_category)

calbevs_plot <- ggplot(data=starbucks, aes(x=reorder(Beverage_category,Calories,FUN=median), y=Calories, fill=reorder(Beverage_category, Calories, FUN=median)))+geom_boxplot()+xlab("Beverage Category")+theme(axis.text.x = element_blank())+scale_fill_discrete(name="Beverage Category")


