install.packages("psych")
install.packages("scatterplot3d")
install.packages("gvlma") ##the gvlma package is useful for checking 
install.packages("ggplot2")
install.packages("rgl")
install.packages("rcmdr")
install.packages("car")
library(psych)
library(scatterplot3d)
library(gvlma)
library(ggplot2)
library(rgl)
library(rcmdr)
library(car)
data("iris")

##For the purpose of this workshop we won't worry about analyzing the iris data - instead we are just going to focus on visualizing it.

##Let's visualize the relationship between Sepal.Width and Species using the base R package.

boxplot(Sepal.Length~Species, main="Iris Sepal Lengths by Species", data=iris)
## That was easy! What would it look like in ggplot2?


##First, let's set up our aes mappings, theme, and general layout so we can use it again later

graphlayout<-ggplot(iris,aes(x=Petal.Length,y=Sepal.Length, group=Species))+ 
  theme_light()
##try different themes! theme_light(), theme_bw(), theme_gray()

graphlayout_customtheme<-ggplot(iris, aes(x=Petal.Length,y=Sepal.Length, group=Species))+ 
  theme(panel.background=element_rect(fill="white", colour="black"), plot.title = element_text(hjust=0.5))+ 
  labs(x="Flower Petal Length", 
       y="Flower Sepal Length",
       title="Relation between Petal & Sepal Length")

graphlayout+geom_boxplot()
graphlayout+geom_point()

##Colour coding: you can change the colour of the data based on grouping variables by adding in aes mappings in your geoms

graphlayout_customtheme+geom_boxplot(aes(colour=Species))
graphlayout_customtheme+geom_point(aes(colour=Species))



##We can change the coordiantes of our grid quite easily

graphlayout_customcoordinates<-ggplot(iris,aes(x=Petal.Length,y=Sepal.Length)) +
  theme(panel.background=element_rect(fill="white", colour="black"))+ 
coord_cartesian(xlim=c(0,20), ylim=c(0,10), expand=FALSE)

graphlayout_customcoordinates+geom_point(aes(colour=Species))

## The axis titles have been automatically populated by your data names, but we can change that!

graphlayout_customlabels<-ggplot(iris,aes(x=Petal.Length,y=Sepal.Length, group=Species)) +
  theme(panel.background=element_rect(fill="white", colour="black"))+ 
  coord_cartesian(xlim=c(0,8), ylim=c(0,8), expand=FALSE)+
  labs(x="Flower Petal Length", 
     y="Flower Sepal Length",
     title="Relation between Petal & Sepal Length")

graphlayout_customlabels+geom_point(aes(colour=Species))


##Creating lines of best fit were one of the trickiest things for me to work out and, in my opinion, is one of the clearest illustrations of how ggplot2 differs
##from base R packages. Basic R plots will automatically fit a line of best fit whereas ggplot requires you to specify the intercepts and slopes of the relationship 
##you want to model and draw a line on your graph.

PetalSepalLengths<-with(iris, lm(Sepal.Length~Petal.Length))
coefficients(PetalSepalLengths)
## So the intercept is 4.31, and the slope is 0.41
bestfit=geom_abline(slope=0.41, intercept=4.31)
graphlayout_customlabels+geom_point(aes(colour=Species))+bestfit

##let's save it!
graphlayout_customlabels+geom_point(aes(colour=Species))+bestfit
ggsave("mygraph.png",dpi=300)
dev.off

##What if we had multiple predictors and wanted to see the contribution of each predictor, not just the model?
##Making the graph in 3d
mygraph<-with(iris, scatterplot3d(Sepal.Length, Sepal.Width, Petal.Length, 
                     ylab="Petal Length",
                     xlab="Sepal Length",
                     zlab="Sepal Width"))
##In order to create a plane (instead of line of best fit) we need to create the linear model
linearmodel<-with(iris, lm(Sepal.Width~Sepal.Length+Petal.Length))
mygraph$plane3d(linearmodel)

##making the graph spin with RGL!
with(iris, scatter3d(Sepal.Length, Sepal.Width, Petal.Length, 
                         ylab="Petal Length",
                         xlab="Sepal Length",
                         zlab="Sepal Width",
                         parallel=TRUE))
##We can also make some customizations
with(iris, scatter3d(Sepal.Length, Sepal.Width, Petal.Length, groups=iris$Species,
ylab="Petal Length",
xlab="Sepal Length",
zlab="Sepal Width",
parallel=FALSE))
##The plains placed by RGL is automatically the plane of best fit, and assumes a linear plane. This can be changed
with(iris, scatter3d(Sepal.Length, Sepal.Width, Petal.Length, groups=iris$Species,
                     ylab="Petal Length",
                     xlab="Sepal Length",
                     zlab="Sepal Width",
                     parallel=TRUE, fit="smooth"))
