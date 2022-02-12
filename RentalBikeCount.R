# set the directory
setwd("C:/Users/HP/Desktop/NCI - MS Data Analytics/Semester 1/Data Mining and Machine learning 1/Project data set")
# get the directory in console
getwd()
# read the file and save in bikerentalhoursdata
bikerentalhoursdata <- read.csv("hour.csv", header=T, na.strings=c(""), stringsAsFactors = T)
# get the structure and some rows of data
head(bikerentalhoursdata)
# summary of the data includes descriptive statistics
summary(bikerentalhoursdata)
# structure of the data
str(bikerentalhoursdata)
# The unique values of each column can be identified 
rapply(bikerentalhoursdata,function(x)length(unique(x)))
#The total number of rows and columns can be verified using the below function
dim(bikerentalhoursdata)
#verify that are there any null values in the dataset
sum(is.na(bikerentalhoursdata))
colSums(is.na(bikerentalhoursdata))
# rename the variables using tidyverse library
library(tidyverse)
bikerentalhoursdata <- bikerentalhoursdata %>% rename(date = dteday,year = yr,month = mnth, hour = hr, weather = weathersit, humidity = hum, count = cnt)
# verify the columns are renamed
colnames(bikerentalhoursdata)
# Year and month columns are already available separately; date column can be removed with instant 
#can have subset named "bikerentalhoursdata1" in the place of original data "bikerentalhoursdata"
bikerentalhoursdata1 = subset(bikerentalhoursdata, select = -c(date,instant,casual,registered))
# verify the column names are removed
colnames(bikerentalhoursdata1)
# changing variables in to categorical variables
bikerentalhoursdata1$season = as.factor(bikerentalhoursdata1$season)
bikerentalhoursdata1$hour = as.factor(bikerentalhoursdata1$hour)
bikerentalhoursdata1$holiday = as.factor(bikerentalhoursdata1$holiday)
bikerentalhoursdata1$weekday = as.factor(bikerentalhoursdata1$weekday)
bikerentalhoursdata1$workingday = as.factor(bikerentalhoursdata1$workingday)
bikerentalhoursdata1$weather = as.factor(bikerentalhoursdata1$weather)
bikerentalhoursdata1$year = as.factor(bikerentalhoursdata1$year)
bikerentalhoursdata1$month = as.factor(bikerentalhoursdata1$month)
# verify the variables changed to factors
str(bikerentalhoursdata1)
# verify the outliers using box plot
boxplot(bikerentalhoursdata1) 
# IQR*1.5(IQR - Inter Quartile Range) Rule can be used to remove the outliers
# This rule can be applied to the non-factor variables which are "count", "humidity" and "windspeed"
Q1_cnt <- quantile(bikerentalhoursdata1$count, .25)
Q3_cnt <- quantile(bikerentalhoursdata1$count, .75)
IQR_cnt <- IQR(bikerentalhoursdata1$count)
Q1_windspeed <- quantile(bikerentalhoursdata1$windspeed, .25)
Q3_windspeed <- quantile(bikerentalhoursdata1$windspeed, .75)
IQR_windspeed <- IQR(bikerentalhoursdata1$windspeed)
Q1_humidity <- quantile(bikerentalhoursdata1$humidity, .25)
Q3_humidity <- quantile(bikerentalhoursdata1$humidity, .75)
IQR_humidity <- IQR(bikerentalhoursdata1$humidity)
bikerentalhoursdata1 <- subset(bikerentalhoursdata1, bikerentalhoursdata1$count > (Q1_cnt - 1.5*IQR_cnt) & bikerentalhoursdata1$count < (Q3_cnt + 1.5*IQR_cnt)
                               & bikerentalhoursdata1$windspeed > (Q1_windspeed - 1.5*IQR_windspeed) & bikerentalhoursdata1$windspeed < (Q3_windspeed + 1.5*IQR_windspeed)
                               & bikerentalhoursdata1$humidity > (Q1_humidity - 1.5*IQR_humidity) & bikerentalhoursdata1$humidity < (Q3_humidity + 1.5*IQR_humidity))
# verify the extreme outliers are removed
boxplot(bikerentalhoursdata1)
# verify the count of the data
dim(bikerentalhoursdata1)
colnames(bikerentalhoursdata1)
# to check the correlation matrix (correlation between variables)
#pairs(bikerentalhoursdata1,panel=panel.smooth)

# removing the variables atemp and holiday
bikerentalhoursdata2 = subset(bikerentalhoursdata1, select = -c(atemp,holiday))
# verify the count of the data
dim(bikerentalhoursdata2)
colnames(bikerentalhoursdata2)
# once again check the structure of the data
str(bikerentalhoursdata2)

# The dummy variable can be created for the categorical variables such as "season", "year", "month", 
# "hour", "weekday", "workingday" and"weather". 
# A dummy variable is a numeric variable that represents the categorical data
library(fastDummies)
bikerentalhoursdata2 <- dummy_cols(bikerentalhoursdata2, select_columns = c("season", "hour", "weekday", "workingday","weather","month","year"), 
                                   ignore_na = TRUE, 
                                   remove_first_dummy = TRUE,
                                   remove_most_frequent_dummy = FALSE,
                                   split = NULL,
                                   remove_selected_columns = TRUE) 
# verify the newly created dummy variable columns and validating the null values in the variables
colSums(is.na(bikerentalhoursdata2))
library(ggcorrplot)
corr <- round(cor(bikerentalhoursdata2), 10)
ggcorrplot(corr)

# caTools library is used to split the dataset in to train and test set
library(caTools)
# every time same set of training and testing data will be taken
set.seed(123)
# split the data in to 60 percent training set and 40 percent testing set
sample.split(bikerentalhoursdata2$count, SplitRatio = 0.60) -> split_tag
# number of training and testing set count split
table(split_tag)
# store the training data in the train set
subset(bikerentalhoursdata2, split_tag==T) -> train
# store the testing data in the train set
subset(bikerentalhoursdata2, split_tag==F) -> test
# row count of training data
nrow(train)
# row count of testing data
nrow(test)
# Apply multiple regression model using lm() function in train data
model1<-lm(count~., data = train)
# summary of the model
summary(model1)
# backward selection process
# remove month_10 in model2 updating model1 as the variable is not significant
model2 <- update(model1,~. -month_10)
summary(model2)
# remove month_4 in model3 updating model2 as the variable is not significant
model3 <- update(model2,~. -month_4)
summary(model3)
# remove month_9 in model4 updating model3 as the variable is not significant
model4 <- update(model3,~. -month_9)
summary(model4)
# remove weather_4 in model5 updating model4 as the variable is not significant
model5 <- update(model4,~. -weekday_5)
summary(model5)
# remove weekday_5 in model6 updating model5 as the variable is not significant
model6 <- update(model5,~. -weather_4)
summary(model6)
# remove month_12 in model7 updating model6 as the variable is not significant
model7 <- update(model6,~. -month_12)
summary(model7)
# to verify the Gauss Markov's Assumptions - linearity, homoscedasticity, outliers, errors are normally distributed
par(mfrow=c(2,2))
plot(model7)

# We can take log of the dependent variable and follow the same steps as we did above 
# (from the model 1) and verify the results using train data
model1<-lm(log(count)~., data= train)
summary(model1)
model2 <- update(model1,~. -weather_4)
summary(model2)
model3 <- update(model2,~. -month_8)
summary(model3)
model4 <- update(model3,~. -weekday_5)
summary(model4)
model5 <- update(model4,~. -month_10)
summary(model5)
model6 <- update(model5,~. -month_7)
summary(model6)
# verify the assumptions plot
par(mfrow=c(2,2))
plot(model6)
# multicollinearity check on the variables using car library
library(car)
vif(model6)
# predict the values using model6 in the test dataset
predict(model6, newdata=test) ->count_result
# The predicted result for count is in "count_result". Apply log for the count column in test data set 
#and bind it with "count_result" as below and store it in final_result
cbind(Actual = log(test$count), predicted = count_result) -> final_result
# The class of the "final_result" is matrix-array. Converting the matrix-array to data.frame
View(final_result)
class(final_result)
as.data.frame(final_result) -> final_result
class(final_result)
# Calculating the error between predicted count and actual count and store it in "final_error"
(final_result$Actual-final_result$predicted) -> final_error
# bind the "final_result" and "final_error" as below and store it in "final_result"
cbind(final_result,final_error) -> final_result
# to view actual, predicted and final_error results 
View(final_result)
# to find the root mean square error
sqrt(mean((final_result$final_error)^2)) -> rmse_final
rmse_final
#actuals_and_predictions <- data.frame(cbind(actuals=bikerentalhoursdata2$count, predicteds=my_prediction))
# using caret adjusted r2, MAE, RMSE can be calculated with the below functions
library(caret)
R2(count_result,log(test$count)) ->adjustedr2
adjustedr2


#Random Forest
# with mtry = p/3 = 52/3 = 17.33 (automatically it will take)
library(randomForest)
# randomForest() function predicts the p value as 17. Apply random forest model using training data
rf2.bikerentalcount <- randomForest(log(count)~., data=train, importance = TRUE)
rf2.bikerentalcount
# predict the output using test data
rf3.bikerentalcount <- predict(rf2.bikerentalcount, newdata=test)
rf3.bikerentalcount
# means squared error of dependent variable using test data
mean((rf3.bikerentalcount-log(test$count))^2)
# Using the "tuneRF" function, we shall find the best mtry 
#whereas "mtry" defines the number of variables sampled randomly as candidates in each split
bestmtry <- tuneRF(train, train$count, stepFactor = 1.2, improve =0.01, trace= T, plot = T)
print(bestmtry)
# randomForest function is used to build random forest model 
#in the train data set with mtry value 39 and importance (important variables used in the model)
# from the tune RF function we got the mtry value of 39 which is having the less OOB error
rf.bikerentalcount <- randomForest(log(count)~., data=train, mtry = 39, importance = TRUE)
rf.bikerentalcount
# predict the output using test data
rf.bikerentalcount1 <- predict(rf.bikerentalcount, newdata=test)
rf.bikerentalcount1
# means squared error of dependent variable using test data
mean((rf.bikerentalcount1-log(test$count))^2)
# to check the important variables
importance(rf.bikerentalcount)
# plot tells which variables are important
varImpPlot(rf.bikerentalcount)
# predict the values using random forest model in the test dataset
predict(rf.bikerentalcount, newdata=test) ->count_result
# The predicted result for count is in "count_result". Apply log for the count column in test data set 
#and bind it with "count_result" as below and store it in final_result
cbind(Actual = log(test$count), predicted = count_result) -> final_result
View(final_result)
class(final_result)
as.data.frame(final_result) -> final_result
class(final_result)
# Calculating the error between predicted count and actual count and store it in "final_error"
(final_result$Actual-final_result$predicted) -> final_error
# bind the "final_result" and "final_error" as below and store it in "final_result"
cbind(final_result,final_error) -> final_result
# to view actual, predicted and final_error results
View(final_result)


plot(rf.bikerentalcount1,log(test$count))
abline(0, 1)


