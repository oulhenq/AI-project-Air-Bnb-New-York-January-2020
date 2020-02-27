##### AI PROJECT : NEW YORK AIRBNB #####

## HUGO BOUTET - QUENTIN OULHEN
## MASTER 1 // IA School - January/February 2020

### Import data and uploading the libraries ###

library(readr)
data <- read_csv("Desktop/IA SCHOOL/Cours/Semestre 1/PROJET IA/AB_NYC_2019.csv")
# don't forget to change the path !

# View(data) # to check the dataset

#install.packages("data.table")
library(data.table)
# to have the isoweek function

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#install.packages("Hmisc")
library(Hmisc)

library(corrplot)
library(RColorBrewer)
library(ggplot2)

#install.packages("leaps")
library(leaps)
# for BIC (function "regsubsets")

#install.packages("lme4")
library(lme4)

#install.packages("Metrics")
library(Metrics)
# to use the rmse function

#install.packages("DAAG")
library(DAAG) # not working on my version of R unfortunately !

#install.packages("xgboost")
library(xgboost) # not working on my version of R

install.packages("class")
library(class) # not working neither
# used for the knn algorithm


### Data preparation ###

## Duplication data

nrow(data) # 48895
# 48895 id differents : so no duplication data

## Aberant data

# Removing the individuals with price equal to 0

nr <- nrow(data)
data_temp <- data # creation of a temporary dataset to proceed

j <- 1
k <- 0

for (i in 1:nr) {
  
  if (data$price[i] == 0) {
    data_temp <- data_temp[-j,]
    k <- k +1
  }
  else {
    j <- j + 1
  }
}

difference <- nr - k
nrow(data_temp) - difference # = 0 : OK !

data <- data_temp
nrow(data) # 48884
# meaning 11 individuals with price equal to 0 (as we had 48895 at start)

str(data)
levels(data$room_type)

dim(data)

summary(data) # explained more clearly in the "statistics" section
# Just to have an overview here.
# Some price at 10,000 $ a night, still possible.
# Some individuals with minimum nights number bigger than a year, but we can't say it's aberant

## Rename the variables

# Numeric variables (quantitatives)

id <- data$id # not useful in our analysis
host_id <- data$host_id # not useful in our analysis
latitude <- data$latitude
longitude <- data$longitude
price <- data$price
minimum_nights <- data$minimum_nights
number_of_reviews <- data$number_of_reviews
reviews_per_month <- data$reviews_per_month
calculated_host_listings_count <- data$calculated_host_listings_count
availability_365 <- data$availability_365

# Date format : last_review in week number (ISO_week)
last_review <- isoweek(data$last_review)
# Use of isoweek function to get the week number.
# We choose to put it by week because we think it's relevant,
# we can differentiate the vacation weeks for example.
# Another idea could have been to split the week days
# and the week-ends.

# Categorical variables (qualitatives/catégorielles)
name <- data$name # not useful in our analysis
host_name <- data$host_name # not useful in our analysis
neighbourhood_group <- data$neighbourhood_group
neighbourhood <- data$neighbourhood
room_type <- data$room_type

## Creation of two data frames with the variables we want (p : part, f : full)

# A first one containing only the numeric variables 
# including the date in week number

data.p <- data.frame(
  
  price,
  latitude,
  longitude,
  minimum_nights,
  number_of_reviews,
  reviews_per_month,
  calculated_host_listings_count,
  availability_365,
  last_review
  
)

# A second one including all the variables

data.f <- data.frame(
  
  price,
  latitude,
  longitude,
  minimum_nights,
  number_of_reviews,
  reviews_per_month,
  calculated_host_listings_count,
  availability_365,
  last_review,
  neighbourhood_group,
  neighbourhood,
  room_type
  
)


## Looking at the NAs values : deleting the full individual/row
# using na.omit function
# This in order to prepare the dataset for the modeling part.
# We use this method as the dataset still relatively "big enough"
# to compute a model even by deleting the rows with a NA.

dim(data) # 48884 rows and 16 columns
dim(data.p) # 48884 rows and 9 columns
dim(data.f) # 48884 rows and 12 columns

data.p <- na.omit(data.p)
data.f <- na.omit(data.f)

nrow(data.p) # 38833
nrow(data.f) # 38833
# No difference between the two dataframes

# View(data.p)
# View(data.f)

## Second variables rename (to simplify variables use)
# Numeric variables (quantitatives)

price <- data.f$price
latitude <- data.f$latitude
longitude <- data.f$longitude
minimum_nights <- data.f$minimum_nights
number_of_reviews <- data.f$number_of_reviews
reviews_per_month <- data.f$reviews_per_month
calculated_host_listings_count <- data.f$calculated_host_listings_count
availability_365 <- data.f$availability_365

# Date format : last_review in week number

last_review <- data.f$last_review

# Categorical variables (qualitative/catégorielle)

neighbourhood_group <- data.f$neighbourhood_group
neighbourhood <- data.f$neighbourhood
room_type <- data.f$room_type

### Some statistics about the dataset ###

head(data.f) # show the first individuals of the dataset
summary(data.f)
# gives some statistics about the numeric variables
# but also the frequency of the categorical ones

str(data.f)
# Gives the type of variable 
# with the values taken by the first individuals
# and gives the levels of the categorical variables.
# It's also possible to use the levels function for this last comment : levels(my_var)

# Statistics about our variable of interest
summary(data.f$price)
# Mean 142.4 and Median at 101.0, maximum value at 10000.0
# The mean is a a great deal bigger than the median, meaning a positively skewness of Price variable
sd(data.f$price)
# standard deviation is 196.9577

par(mfrow = c(1,2))
# display the plots in the same page
# for exampe "par(mfrow = c(2,2))"  display 4 graphs

hist(data.f$price, breaks = 50, xlab = "Price", col = "Blue", main = "Histogram of Price")
hist(log(data.f$price), breaks = 30, xlab = "Price", col = "Blue", main = "Histogram of Log(Price)")
# We can see that the log of price is more likely to follow a normal distribution


par(mfrow = c(1,1))

# scatterplot example
scattdf <- data.frame(price, number_of_reviews)
plot(scattdf,  xlab = "Price", ylab = "Number of reviews",
     main = "Plot of Price against Number of reviews")
# The plot of Price against Number of reviews show that 
# it seems there is no relationship between the two variables

par(mfrow = c(1,2))
# QQPlot of the price
qqnorm(data.f$price, main = "Price QQ Plot")
qqline(data.f$price, col='blue')
# unlikely to have a normal distribution

# QQPlot of the log of the price
qqnorm(log(data.f$price), main = "Log of price QQ Plot")
qqline(log(data.f$price), col='blue')
# the QQplot shows that the curvature deviate slightly at the upper
# but more likely to follow a normal distribution however

par(mfrow = c(1,1))

# Example of a QQPlot for another variable of the dataset
qqnorm(data.f$number_of_reviews, main = "Number of Reviews QQ Plot")
qqline(data.f$number_of_reviews, col='blue')
# plot of theroretical versus sample quantile don't seems to be a normal distribution at all

# Creation of three data frames 
# in order to plot the correlations and the variable to be explained (price) 
# against all the explanatory variables
# with first aim to make the plots more readable

data.price.num1 <- data.frame (
  
  price,
  latitude,
  longitude,
  number_of_reviews,
  reviews_per_month,
  last_review
  
)

data.price.num2 <- data.frame (
  
  price,
  minimum_nights,
  availability_365,
  calculated_host_listings_count
  
)

data.price.cate <- data.frame (
  
  price,
  neighbourhood_group,
  neighbourhood,
  room_type
  
)

## Correlations
# Correlation only between numerical (quantitatives) variables of course !

# chart.Correlation(data.p, hitsogram=TRUE, pch=19) # too long (on my computer) and unreadable
chart.Correlation(data.price.num1, hitogram=TRUE, pch=19)
# weak negative correlation of price with longitude (-0.16)
# additionnal information :
# strong positive correlation between number_of_reviews and reviews_per_month (0.55)
# as we can expect !
chart.Correlation(data.price.num2, hitogram=TRUE, pch=19)
# not explicit correlation for price variable here

# pairs(data.f) # display is not readable ! 
# Better splitting and show it in three times
pairs(data.price.num1)
pairs(data.price.num2)
pairs(data.price.cate)
# The pairwise relationships above shows the relationship of the price variable 
# upon each of the others explanatory variables
# with additional information given by the relationships 
# between the explanatory variables amongst themselves. 
# From the plots, it seems there is no relationship between the price
# and the others explanatory variables

M <- cor(data.p)

corrplot(M, type="upper", order = "original", tl.col = "black", tl.srt = 45, col=brewer.pal(n=6, name="RdYlBu"))
# seems to have a noticebale negative correlation between the price and latitude

### Splitting Train and Test ###
# should have been done before doing the data preparation to avoid introducing a bias 
# but in our case we decided to do it after the data preparation 
# as the step was quite quick (no major changes)

# View(data.f)

# Random sample indexes
train_index <- sample(1:nrow(data.f), 0.7 * nrow(data.f))
test_index <- setdiff(1:nrow(data.f), train_index)

length(train_index) # 27183
length(test_index) # 11650

# length(train_index) + length(test_index) - nrow(data.f) = O : Ok !

train <- data.f[train_index,]
test <- data.f[test_index,]

### Multiple Linear Regression ###
# variable to be explained : price 

## Modeling part using train
model.lm <- lm(price~., data = train)

summary(model.lm)

?step
# step will use the AIC citerion to proceed to the differents steps
# if the smallest AIC value from the dataset variables is lower than the criterion
# then the variable will be remove from the model.
# Better than checking the p-values from a statistic point of view.

slm <- step(model.lm, direction = "backward", trace = FALSE)
# using step function to do the backward elimination method
# direction can either be "forward", "backward" or "both"
# We suggest using "backward" elimination (best one in our opinion)
# trace = true : means all the steps of the calculation will be displayed

summary(slm)
slm$anova # gives all the variables remove from the step function above
# Backward selection using AIC criterion suggest remove :
# neighbourhood_group, reviews_per_month and latitude

# View(train_lm)

# Model selection based on others criterion
model.crt = regsubsets(price ~ latitude + longitude + minimum_nights +
                           number_of_reviews + reviews_per_month +
                           calculated_host_listings_count + availability_365 +
                           last_review, method = "exhaustive", data = train)
plot(model.crt, scale="bic") # BIC : Bayesian Inference Criterion
# keep numeric variables : latitude, longitude, number_of_reviews, availability_365

plot(model.crt, scale="Cp")
plot(model.crt, scale="adjr2") # Adjusted R squared
# keep numeric variables : latitude, longitude, minimum_nights, number_of_reviews, calculated_host_listings_count, availability_365, last_review

# Models selection based on others criterion
model.crt2 = regsubsets(price ~ neighbourhood_group + room_type, method = "exhaustive", data = train)

plot(model.crt2, scale="bic")
# suggest keep : 2/4 neighbourhood_group and 2/2 room_type
# not possible to implement it for neighbourhood variable
# as there are too many levels (more than 200 : impossible to show on the graph !)
# however we can imagine BIC would have suggest keep neighbourhood instead of neighbourhood_group
# as we can have the information of neighbourhood_group with neighbourhood

# Conclusion, trying as follow:
# a first model with (room_type) only
# a second one with (neighbourhood, room_type)

plot(model.crt2, scale="Cp")
plot(model.crt2, scale="adjr2")

# suggest keep : 3/4 from neighbourhood_group (so neighbourhood instead) and 2/2 room_type

# Conclusion keep variables : neighbourhood, room_type

bic.model1 <- lm(price ~ latitude + longitude + number_of_reviews + availability_365 + room_type, data = train)

bic.model2 <- lm(price ~ latitude + longitude + number_of_reviews + availability_365 + neighbourhood 
                   + room_type, data = train)

adjr2.model <- lm(price ~ latitude + longitude + minimum_nights + number_of_reviews + calculated_host_listings_count
                    + availability_365 + last_review + neighbourhood + room_type, data = train)

# Both models include results from BIC and Adjusted R squared
# from model.crt : regarding the numerical variables
# from model.crt2 : regarding the factor variables

### Fitting the Model ###

fit1 <- model.lm
fit2 <- slm
fit3 <- bic.model1
fit4 <- bic.model2
fit5 <- adjr2.model

BIC(fit1, fit2, fit3, fit4, fit5)

### Comparing Models ###
## Use of function anova to see which of the model is best

anova(fit2, fit1) # p-value > 0.5 : keep fit2
anova(fit3, fit2) # p-value < 0.5 : keep fit2
anova(fit4, fit2) # p-value < 0.5 : keep fit2
anova(fit2, fit5) # p-value > 0.5 : keep fit2

anova(fit2)

summary(fit2)

# Best fit : fit2

fit <- lm(formula = price ~ longitude + minimum_nights + number_of_reviews + 
            calculated_host_listings_count + availability_365 + last_review + 
            neighbourhood + room_type, data = train)

### Diagnostic Plots ###

par(mar=c(1,1,1,1))
par(mfrow=c(2,2)) # 4 graphs/page (optional)
# equivalent to : layout(matrix(c(1,2,3,4),2,2))

plot(fit)
# Warnings messages when plotting saying that
# some observations with leverage one are not plotted

## Trying some useful functions on the final fit
summary(fit)
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
# influence(fit) # regression diagnostics

# outlierTest(fit2) # function not available on this version of R
# qqPlot(fit2) # function not available on this version of R

summary(fit)

pred <- predict.lm(object = fit, newdata = test)
# run a first time the predict.lm and check if there is a unused level in the test data
# if it's the case we need to remove them using the following line of code :
# test$neighbourhood <- droplevels(test$variable_name, c=("level1_name","level2_name",...))
# drop the unused levels from factor neighbourhood using droplevels function
# BE CAREFUL : The unused levels factor can be different from a train test split to another

pred <- predict.lm(object = fit, newdata = test)

length(test$price)
length(pred)
# both have 11650 rows

diff <- test$price - round(pred)
summary(diff)
# 2 NA's in pred : we need to remove them

# The following step is not useful if you didn't have to remove levels
testdf <- data.frame(test$price, pred)
testdf <- na.omit(testdf)
nrow(testdf) # To check if the number of rows is coherent with what you removed

# If you DIDN'T have to remove levels, use the following line of code :
rmse(test$price, pred)

# Otherwise use next line :
rmse(testdf$test.price, testdf$pred)

# The first time I ran it, Root Mean Squared Error was 185.2137.
# Conclusion : it was the best model.
# We probably would have had a better result using Ridge or XGBoost
# choosing the same variables used to fit the train in R.
# But unfortunately packages are not working.

write.csv2(train, "desktop/airbnb_train_data.csv")
write.csv2(test, "desktop/airbnb_test_data.csv")
# to export the train and test datasets for analysis in Python
# in order to compute the XGBoost and knn algorithms

### THE END ###
