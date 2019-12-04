
#data cleaning and exploration 
#jaky kueper
#december 3, 2019
#R Ladies, London ON


#if you want to follow along, these are the required packages
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("naniar")
install.packages("stringr")
install.packages("purrr")

#load packages 
library(readxl)
library(dplyr)
library(tidyr)
library(naniar)
library(stringr)
library(purrr)



# BASIC DF STRUCTURE 

#vectors 
vec1 <- c(1,3,5,8)
vec2 <- c('a','b','r','h')
vec3 <- c('hi there', 'we are', 'creating', 'some data!')

#we can combine individual vectors into a dataframe
df <- data.frame(vec1, vec2, vec3)

#character vectors were automatically converted to factor 
df2 <- data.frame(vec3, vec2)
df2 <- data.frame(vec3, vec2, stringsAsFactors = FALSE)

#to combine dataframes we can use cbind ("column bind")
#useful for appending information to an existing df 
df3 <- cbind(vec1, df2)



#Practice with data
#data used can be downloaded here: https://docs.google.com/spreadsheets/d/1oJBaH2x369leRtL19HD8n1oZlGqwK9Yz9b14ppUEhXg/edit#gid=439114268
#more info and other practice sets: https://makingnoiseandhearingthings.com/2018/04/19/datasets-for-data-cleaning-practice/

#read in data 
df <- read_excel("dataCleaningAndExplorationMay30Lead/dataCleaning/G&T Results 2017-18 (Responses).xlsx")

#get dimensions of the data: row * column
dim(df)

#look at the data - there are more approaches than shown here
View(df) #we can see each person has a row

#overview of all columns - can see NAs
summary(df)

#get listing of columns, types, values 
glimpse(df)

#Notice some test scores are unexpectedly character - we will look at how to deal with this later

#first re-name 
#remame where the col name is 'NNAT Non Verbal Raw Score'
names(df)[names(df) == 'NNAT Non Verbal Raw Score'] <- 'NNATNonVerbalRawScore'

#or using column index 
names(df)[1] <- 'Time'

#or do a bunch at once with dplyr
df <- df %>%
  rename(
    EnteringGradeLevel = 'Entering Grade Level',
    BirthMonth = 'Birth Month',
    OLSATVerbalScore = 'OLSAT Verbal Score',
    OverallScore = 'Overall Score',
    Enroll = 'Will you enroll there?'
    
  )

#you can try the rest on your own :) 


#now let's look at suspicious variables
table(df$NNATNonVerbalRawScore)
table(df$OLSATVerbalScore)


#let's replace non numeric values with NA 
#write out all the values you want to be considered NA
listNA <- c('-', '**', 'Fill out later.')

df <- df %>%
  replace_with_na(replace = list(NNATNonVerbalRawScore = listNA,
                                 OLSATVerbalScore = listNA))

#check
table(df$NNATNonVerbalRawScore)
table(df$OLSATVerbalScore)

#we could have instead repeated this code for all instances 
# df$NNATNonVerbalRawScore[df$NNATNonVerbalRawScore == '-'] <- NA 

#the above worked, but we still see weird values

#perhaps these are tests out of 48 and 30 (I should talk to someone to confirm)
#some entries have the denominator number and a slash
#if we convert to numeric these entries will become NA
#we can use Stringr to remove the second part of the entry! 
#this is one approach 

#the split function will take an entry and split it at a specified symbol
#let's look at it on our column of interest
str_split(df$NNATNonVerbalRawScore, "/")

#to save only the first part of each split in the df, we can combine with purrr
#first, make a function that searches for '/' in a  given parameter, and splits if found
takeNumerator <- function(entry) {
  pieces <- str_split(entry, "/")
  return (pieces[[1]][1])
  
}

#second, apply this function to each entry of the column, using purrr
#note you may want to save as a new var to avoid needing to restart :) 
df$NNATNonVerbalRawScore <- map(df$NNATNonVerbalRawScore, takeNumerator)
df$OLSATVerbalScore <- map(df$OLSATVerbalScore, takeNumerator)

#check the type that was returned 
typeof(df$NNATNonVerbalRawScore)

#change to nunmeric entries
df$NNATNonVerbalRawScore <- as.numeric(unlist(df$NNATNonVerbalRawScore))
df$OLSATVerbalScore <- as.numeric(unlist(df$OLSATVerbalScore))

#explore a histogram
hist(df$NNATNonVerbalRawScore)

#we see that some values are over 48!
#these could be percentile misentries 
#or someone entered the result of dividing the numerator by denominator (let's pretend it's this)

#to fix, we could use a similar approach to above
#write another function and apply it using purrr
#or we could use a for loop 

#first let's look ahain at our starting point
#we expect 5 values to be changed
table(df$NNATNonVerbalRawScore)


for (i in 1:118) {
  if (df$NNATNonVerbalRawScore[i] > 48 & ! is.na(df$NNATNonVerbalRawScore[i])) {
    df$NNATNonVerbalRawScore[i] <- as.integer(df$NNATNonVerbalRawScore[i] / 48)
    print(i)
  }
}

#look again
table(df$NNATNonVerbalRawScore)
hist(df$NNATNonVerbalRawScore)


#for our other var the denom looks like 30
#note if we had gone the function route we would have minimized duplicate code
#e.g. pass the denominator and column as parameters
#we can reuse some of the above for OLSAT score
table(df$OLSATVerbalScore)

for (i in 1:118) {
  if (df$OLSATVerbalScore[i] > 30 & ! is.na(df$OLSATVerbalScore[i])) {
    df$OLSATVerbalScore[i] <- as.integer(df$OLSATVerbalScore[i] / 30)
    print(i) #note this line is just a check
  }
}



############
############

#Above we saw one approach to removing non-standard NA values and fixing weird values

#here is another approach using the Enrollment character variable as an example
#maybe we only want Yes responses for enrollment and convert the rest to NA 
#we also want all Yes's to be spelled the same way so they are considered a single value

table(df$Enroll)

df <- df %>%
  mutate(Enroll = replace(Enroll, Enroll == "Maybe", NA)) %>%
  mutate(Enroll = replace(Enroll, Enroll == "No", NA)) %>%
  mutate(Enroll = replace(Enroll, Enroll == "YES", "Yes"))

table(df$Enroll)

#Other options for handing NA:  https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html

############
############

#another simpler conversion 

#change between types, e.g. character to factor
is.character(df$EnteringGradeLevel)

df$EnteringGradeLevel <- as.factor(df$EnteringGradeLevel)

levels(df$EnteringGradeLevel) #default is alphanumeric

#make K first
df$EnteringGradeLevel <- relevel(df$EnteringGradeLevel, "K")

#could have done in one step
# df$EnteringGradeLevel <- factor(df$EnteringGradeLevel, levels = c("K", "1.0", "2.0", "3.0"))
# http://www.cookbook-r.com/Manipulating_data/Changing_the_order_of_levels_of_a_factor/


############
############

# checking for generic missing values 

#can call on a column. Note there is also an is.nan() version
is.na(df$District)

#probably more helpful is to just get number of na
sum(is.na(df$District))


#remove a column 
df <- select(df, -starts_with('School Assig'))

#make a new column 
df$new <- df$NNATNonVerbalRawScore + df$OLSATVerbalScore

summary(df$new)


############
############

#selecting columns and rows  
df %>%
  select(OverallScore, EnteringGradeLevel) %>%
  filter(EnteringGradeLevel == "K")


#there is so much more you can do! 
#it all depends on what you WANT to do
#hence the importance of starting with a clear plan/goal :) 



