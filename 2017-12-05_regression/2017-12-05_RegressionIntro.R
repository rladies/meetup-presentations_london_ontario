########################################
#####R-Ladies London, Ont###############
#####Introduction to Regression#########
#####Presented by Jacqueline Kueper#####
########################################

#we will use data that is available through R for this session 
#for a refresher on how to load your own data into R, review the past two sessions :) 

##########################
#Simple linear regression 
##########################
###one continuous outcome variable, one explanatory variable 

#Example - we are going to use data on 144 adult cats, over 2kg in body weight
#this data is stored in the MASS package
library(MASS)
#load data
data(cats)
#for more information on the data that we will be using
?cats
#note the units of the variables- this will be important later! 

#Example - cats
#we are interested in whether there is an association between body weight (explanatory) and heart weight (outcome)
#but before running any analyses we want to get acquainted with our data
#are all of our variables in the dataframe?
View(cats)

#obtain descriptive statistics, check for funny/incorrect coding (e.g. m,f & M,F for sex), NA, types of variables, etc 
#note - if you have a lot of variables in your dataframe you likely won't want to summarize them all at once 
summary(cats)

#we might want to check for missing data 
#none in this data (we already knew that from our summary)
sapply(cats,function(x) sum(is.na(x)))

#plot data before running regression 
#pay attention to non-linear relationship, outliers, etc - does linear regression seem reasonable? 
plot(cats$Bwt, cats$Hwt, xlab = "Body Weight (kg)", ylab = "Heart Weight (g)")

#we could also use ggplot to create a scatter plot
library(ggplot2)
ggplot(data = cats, aes(y = Hwt, x = Bwt)) +
  geom_point()

#we can add a fitted linear regression line w SE to this plot
ggplot(data = cats, aes(y = Hwt, x = Bwt)) +
  geom_point() +
  geom_smooth(method = "lm")

#but this does not allow us to explore characteristics of our fitted regression model 

#instead, simple linear regression model can be fit using the "lm" function
#we are storing model results as an 'lm' object
#note default is to omit observations w missing data (none in this data)
mod1_slr <- lm(Hwt ~ Bwt, data = cats)

#we can print our model to get the regression equation
mod1_slr

#we can use 'summary' to get more information about our fitted regression model
summary(mod1_slr)
#how do we interpret the coefficients? Evaluate magnitude and precision (not just the p-value!)
#residuals? model fit? 

#we could use info from summary() to manually calculate confidence intervals around the coefficients
#CI = b1-hat +/- (crit value)*SE(b1-hat)
#but there is also a function that will do that for us
#range of values that we are 95% certain contains the population parameter 
confint(mod1_slr)

#let's explore our model a bit more

#we can view fitted values for each observation (expected Y value, given X value)
#these are obtained by plugging in observed Bwt into the regression equation
#example of how to manually obtain fitted value for observation 1
head(cats)
-0.36 + 4.03*2

#this function will print all of our fitted values to the console
fitted.values(mod1_slr)

#what does the mean of our fitted values equal?
mean(fitted.values(mod1_slr))
mean(cats$Hwt) #expected value of Y

#we can also look at our residuals
#this will show how far each observed data point is from the value estimated by the regression equation
residuals(mod1_slr)
#we can check that they sum to zero
sum(residuals(mod1_slr))

#it is often easier to view residuals in a histogram 
#these should look approx normally distributed w mean of 0
hist(residuals(mod1_slr))

#we can use plots to assess linear regression assumptions
#plot() gives us four plots that can be used to assess assumptions 
#determining whether assumptions are met is not an exact science.
plot(mod1_slr)


###########
#this part is only for if you are feeling a bit adventerous. 
#Just exploring another way that we can learn about our regression model.
#if you are not familiar with these packages and functions, or don't have them installed, you can ignore this.
#earlier we stored the results from our linear regression as an object of type 'lm'
class(mod1_slr)
#the 'broom' package store our lm output into a data frame
install.packages('broom')
library(broom)
#this gives a data frame representation of our model outputs
tidy(mod1_slr)
#this shows us many relevant pieces of info at once
head(augment(mod1_slr))

#we can use piping to see our most influential points
# 'augment' allows us to add columns to the original data set
#'arrange' lets us order the rows by one or more columns in aescending or descending order
#'select' lets us subset by columns
# 'head' shows the first few rows
#note .hat are leverage scores, which may not have large impact on fitted line (influence = leverage+large residual)
library(dplyr)
library(magrittr)
mod1_slr %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  select(Hwt, Bwt, .fitted, .resid, .hat, .cooksd) %>%
  head()
##########
#end of exploration - everyone join back in!


##############################
##Multiple Linear Regression##
##############################
#one continuous outcome variable, >1 explanatory variable

#always run a single variable model before adding additional explanatory variables
#AND have a clear reason for adding variables to your model

#cat example continued
#sex may be responsible for part/all of the association between body weight and heart weight
#we can adjust for sex by adding it to our regression model

#fit a multiple linear regression model 
#we use '+' to add more explanatory variables to the right side of the equation. 
mod2_mlr  <- lm(Hwt ~ Bwt+Sex, data = cats)

#we can use all of the same commands for mod2 as we did for mod1
#we will focus on the summary function
#how has our regression coefficient for body weight changed? Interpretation? 
summary(mod2_mlr)

#confidence intervals
confint(mod2_mlr)

#assess model assumptions 
plot(mod2_mlr)


############################
#Generalized linear models
############################
#let's re-fit our linear regression model using the 'glm' function 
#we select the 'gaussian' distribution b/c approx. normally distributed, continuous outcome
mod2_glm <- glm(Hwt ~ Bwt+Sex, family = "gaussian", data = cats)
mod2_glm
#compare with model fit using lm()
mod2_mlr

#########################
#Logistic Regression 
#########################
#one binary outcome variable, one or more explanatory variable(s)

#We can use the 'glm' function to run logistic regression
#glm can be used very similarly to lm
#the main difference is that we need to include a 'family' argument

#we will use 'Aids2' data from the 'MASS' package
#this contains data on 2843 people diagnosed with AIDS, including info about AIDS dx and death
?Aids2
#load data
data("Aids2")

#as with linear regression, we first want to ensure our data looks okay
View(Aids2)
summary(Aids2)

#our outcome variable of interest is whether people were alive or dead at end of observation period
#to run logistic regression, we need our outcome variable to be a factor
is.factor(Aids2$status)
levels(Aids2$status)
#we can use contrasts to see how this variable will work in reg model
#this shows us that Alive will be used as the reference (0)
contrasts(Aids2$status)

#we are interested is assoc between age of AIDs dx and death
#we might first want to know the number (and calc proportion) of people with the outcome of interest
table(Aids2$status)
1761/(1082+1761)

#we set our family to 'binomial' bc our outcome of interest is binary (follows binomial distribution)
mod3_lr <- glm(status ~ age, family="binomial", data=Aids2)
#similar to lm() we can see our model coefficients
mod3_lr
#and model summary
summary(mod3_lr)
#how do we interpret these regression coefficients? 

#it can be easier to interpret coefficients as odds ratios - how the odds of outcome changes as function of explanatory variable
#to get ORs, exponentiate the regression efficients
exp(coef(mod3_lr))
#how do we interpret the age coefficient? 

#confidence intervals on log-odds scale
confint.default(mod3_lr)

#to get 95% confidence interval for OR estimates
exp(confint.default(mod3_lr))

#we can also obtain the estimated probability of death given specific values of explanatory variables
#this is going to print the predicted probability of death for each observation, given their age of AIDs dx
predict.glm(mod3_lr, type="response")

#we may want to add additional explanatory variables.
#we can do this in a similar way as in multiple linear regression (mod2)

#previously we were not interested in the effect of our additional explanatory variable (sex)
#maybe now we are
#we need to obtain some info to be able to properly interpret results for a categorical covariate
#note - variables must be factors in order for R to know that they are categorical

#categorical explanatory variable with 2 levels
#regression coefficients assess the log odds of outcome for males compared to females 
is.factor(Aids2$sex)
contrasts(Aids2$sex) #females are our reference category

#usually R orders a categorical variable alphabetically
#we can reorder variables to create a new reference category
Aids2$sex_rev <- relevel(Aids2$sex, ref = "M")
contrasts(Aids2$sex_rev)

#categorical explanatory categorical variable with > two levels
is.factor(Aids2$T.categ)
#we can view the number of people with the outcome in each category
table(Aids2$status, Aids2$T.categ)

#R fits k-1 dummy variables, one for each category (# categories = k)
#we can use 'contrast()' to see that 'hs' is our reference category (all 0s)
#for each dummy var xi = 1 if category 'present', xi = 0 otherwise
#the corresponding reg. coef. will be comparing the log odds of outcome for that xi=1 vs ref cat (all other dummy var=0)
contrasts(Aids2$T.categ)

#fit multiple logistic regression model
mod4_mlr <- glm(status~T.categ+age+sex, family = "binomial", data = Aids2)
#obtain model summary 
summary(mod4_mlr)

#we see a Wald test to assess statistical sig of each coefficient
#for cat. var this tests whether the pairwise difference between the coefficient of the ref. class and the other class is diff from 0
#usually we are interested in the overall effect of the categorical explanatory variable
#we can asses this using a likelihood ratio test 
#fit a second model without the categorical predictor of interest
mod4_nocat <- glm(status~age+sex, family="binomial", data=Aids2)
summary(mod4_nocat)
#compare the models with and without the variable of interest
anova(mod4_mlr, mod4_nocat, test='LRT')
#the LRT is stat. sig, sugessting there is an overall stat sig effect of T.categ on outcome

#It may be of interest to use the fitted logistic regression equation
#to estimated the probability of death given specific values of explanatory variables
predict.glm(mod4_mlr, data.frame(T.categ = "blood", sex="M", age=25), type="response")
#to obtain 95% confidence intervals for the probability(y|x,x) estimate
#first we calculate the CI on the logit scale
pi.hat = predict.glm(mod4_mlr, data.frame(T.categ = "blood", sex="M", age=25), type="response", se.fit = TRUE)
ci = c(pi.hat$fit - 1.96*pi.hat$se.fit, pi.hat$fit + 1.96*pi.hat$se.fit)
#then we can convert our CI values to probabilities
exp(ci)/(1+exp(ci))
#we can also use 'predict.glm()' to test our model on new data
#this requires all variables in our model to be present in the newdata. 
#sample code: predict.glm(mod4_mlr, newdata = new.df)


#we can test the overall model fit 
#to test this we will compare our model to a model with only an intercept term
#likelihood ratio tests compare full model with restricted model that omits the explanatory variables we are interested in testing
#hyp0: b1 = b2 = 0
#fit the model with only intercept
mod4.empty <- glm(status~1, family="binomial", data = Aids2)
mod4.empty
##perform LRT using anova to compare full and empty model
#p-values come from chi-square distribution
#be wary of Type I error
anova(mod4_mlr, mod4.empty,test='Chisq')
#note we could follow this pattern to compare model fit for any two regression models
#where one contains a subset of explanatory variables of the other (also linear regression)



#######################################
#Survival Analysis#####################
#######################################
#time-to-event outcome, one or more explanatory variables

#we will be using the package 'survival'
#install
install.packages("survival")
#load
library(survival)

#the 'survival' package contains data on survival of patients with AML that diff by chemotherapy treatment regime
#note the boot package also contains this data
??aml

#cens=1 indicates relapse (event), cens=0 indicates no event (censored). time is length of remission ('survival time'). 
#get acquainted w data
View(aml)
summary(aml)

#we can create a table to view the number of events in each treatment category 
table(aml$x, aml$status)
#we could use logistic regression to estimate prob relapse given treatment group 
#treatment may be 'effective' because it prevents relapse, or because it prolongs time until relapse
#survival analysis allows us to use this time in the analysis
#survival analysis allows people to contribute information to analysis even if loss to follow-up

#survival analysis (non-regression)
#we can compare median survival times for the two groups
aml.surv <- (survfit(Surv(time, status) ~ x, data = aml))
aml.surv
#Kaplan-Meier plot for the two treatment groups
plot(aml.surv, lty = 2:3, ylab="Survival Probability", xlab = "Weeks")
legend(100, .9, c("Maintenance", "No Maintenance"), lty = 2:3)
#we can use a log rank test to compare the two groups on survival
#chi-square and p-value are for null hyp of no diff between the two survival curves
survdiff(Surv(time, status) ~ x, data = aml)

#Cox Proportional Hazards Regression 

#you may add additional explanatory variables in the same way we did earlier (x+x+x...)
#we will only include one explanatory variable today
mod5_cox <- coxph(Surv(time,status) ~ x, data = aml)
#we can print our model
mod5_cox
#or get more info using the summary function
#this provides both non-exp and exp regression coef 
#exp(coef) gives hazard ratio
summary(mod5_cox)


#to assess assumptions of Cox regression 
#you can use the survival and survminer packages
install.packages("survminer")
library(survminer)
#in the interest of time I will leave this as an exercise to do at home


#######################################################################################
#Let's revisit adding explanatory variables to regression models (non-exhaustive list!)
########################################################################################

#we might want to change the units of our explanatory variable (may aid interpretation)
#we could do this by altering or creating a new variable in our dataframe
#we could also use 'pipes' and mutate
#mutate lets us change a variable or create a new one
#ex - change our cat body weight from kg to g
#notice how the regression coefficient changes
library(dplyr)
cats %>%
    mutate(Bwt_new = Bwt*1000) %>%
    lm(Hwt ~ Bwt_new, data=.)


#centering explanatory variables
#subtract the mean 
#one use of centering is to make the intercept meaningful (mean of Y for average value X)
cats$Bwt_cent <- cats$Bwt - mean(cats$Bwt)
#check - our new mean should be zero
summary(cats$Bwt_cent)
#notice how our intercept changes
lm(Hwt~Bwt_cent, data = cats)
#you can also center using scale() to subract the column mean from each obsv
cats$Bwt_centscale <- scale(cats$Bwt, center = TRUE, scale = FALSE)
summary(cats$Bwt_centscale)


#standardized regression coefficients
#we can also use scale to standardize variables 
#this is useful if we want to be able to compare regression coefficients 
#betas are shown in terms of SD units. Can be compared within but not betweeen studies 
#here scale centers and divides the centered column values by column's std. deviation
cats$Bwt_std <- scale(cats$Bwt, center = TRUE, scale = TRUE)
summary(cats$Bwt_std)
#notice how the intercept and estimate change
lm(Hwt~Bwt_std, data = cats)


#interaction terms
#a:b specifies to take interactions of all terms in a with all terms in b
#we also want to include the lower order terms. we could do this manually
#or we can use '*' to to fit interaction and lower order terms at the same time
# a*b = a+b+a:b
#this also works for >2 variable interaction
#keep in mind for interpretation of interaction: synergistic vs antagonistic vs qualitative. Additive vs. multiplicative scale.
#if you are using interaction in regression, think about why
#here I'm just using available variable (not a good reason)
lm(Hwt~Bwt*Sex, data = cats)
lm(Hwt~Bwt+Sex+Bwt:Sex, data = cats)
#we can also add interaction terms to glm models
glm(status~age*sex, family = "binomial", data = Aids2)

# adding ploynomial terms 
# this is sometimes done when the linearity assumption is not met 
# check out this: https://www.r-bloggers.com/fitting-polynomial-regression-in-r/
# note the 'gist' at the bottom




