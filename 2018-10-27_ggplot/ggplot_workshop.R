# ggplot_workshop

# Load libraries----
library(ggplot2)
#install.packages("ggplot2")

# Read in your data ----
bikes <- read.csv("data/bikes_new.csv")

ggplot(bikes) +
     aes(x = temp,
         fill = season,
         alpha = 0.25)+
     geom_density()





# Explore your data ----
View(bikes)
str(bikes)
summary(bikes)
head(bikes)
tail(bikes)




















# First plots ----
# A plot!
p <- ggplot(bikes)+ 
       aes(x = temp, 
           y = count, 
           color = season, 
           size = holiday) +
     geom_point()


# Color options ----
#### Name the colors
p + scale_color_manual(values=c("red", "blue", "yellow", "green"))

#### Reference the [color hex codes](https://www.color-hex.com/)

p + scale_color_manual(values=c("#d80e0e", "#1f0ed8",  "#ecee1e", "#11cc3a"))

#### Use a predefined palate

library(wesanderson)
p + scale_color_manual(values = wes_palette(n = 4, name="Moonrise3"))

p + scale_color_brewer(palette="Set1")






# Geometries (geom)----
# - geom_point()
# - geom_line()
# - geom_bar()
# - geom_violin()
# - geom_boxplot()
# - geom_density()
# - geom_smooth()
# - geom_ridgeline()

# Redefine plot on which we'll build
# If we have continuous x and y: _point, _jitter, _smooth
p_cont <- ggplot(bikes)+ 
     aes(x = temp, 
         y = count, 
         color = season)

p_cont + geom_point()
p_cont + geom_jitter()
p_cont + geom_smooth()


# Categorical x, continuous y----
#    : _boxplot, _violin
p_cat_x <- ggplot(bikes) +
     aes(x = season,
         y = count,
         color = season)

p_cat_x + geom_boxplot()

p_cat_x + geom_violin() +
     geom_jitter(alpha = 0.25)


# Histograms and density plots----
# ... Continuous x, no y (y is count or density)----
p <- ggplot(bikes) +
     aes(x = temp)

p + geom_histogram()
p + geom_density()
p + geom_freqpoly()

p <- ggplot(bikes) +
     aes(x = temp,
         fill = season,
         alpha = 0.25)
p + geom_density()


# ... Continuous x, categorical y----
#install.packages("ggridges")
library(ggridges)
p <- ggplot(bikes)+ 
     aes(x = temp, 
         y = season)

p + geom_density_ridges()


# Summary plots ----
# Summarize data
bikes$season <- factor(bikes$season, levels = c("winter", "spring", "summer", "fall"))


library(plyr)
library(dplyr)
library(tidyverse)

bikes_summary <- bikes %>%
     ddply(.(season, type), 
           summarise, 
           count = mean(count))

View(bikes_summary)

# Plot summary data
ggplot(bikes_summary) +
     aes(x=season, y = count) +
     geom_bar(stat = "identity")

ggplot(bikes_summary) +
     aes(x=season, y = count, fill = type) +
     geom_bar(stat = "identity")+
     facet_wrap(season~type)


ggplot(bikes_summary) +
     aes(x=season, y = count, color = type) +
     geom_point() +
     geom_line(aes(group = type))



ggplot(bikes_summary) +
     aes(x=season, y = count, fill = type) +
     geom_bar(stat = "identity") +
     facet_wrap(season~type)

ggplot(bikes_summary) +
     aes(x=season, y = count, fill = type) +
     geom_bar(stat = "identity") +
     facet_grid(type~season)+
     labs(fill = "Type of bike user")


# Labels and Themes----
p <- ggplot(bikes_summary) +
     aes(x=season, y = count, fill = type) +
     geom_bar(stat = "identity")

p +
     labs(title = "Number of bikes rented per season",
       x = "Season",
       y = "Number of bikes rented",
       fill = "Type of bike rental")

p + 
     theme(axis.title = element_text(size=18),
           axis.text.x = element_text(size=12),
           legend.title = element_text(size=12),
           legend.position = "bottom")










