# set the directory
setwd("C:/Users/HP/Desktop/NCI - MS Data Analytics/Semester 1/Data Mining and Machine learning 1/Project data set")
# get the directory in console
getwd()
# read the file and save in fordcardata
fordcardata <- read.csv("ford.csv", header=T, na.strings=c(""), stringsAsFactors = T)
# get the structure and some rows of data
head(fordcardata)
# summary of the data includes descriptive statistics
summary(fordcardata)
# structure of the data
str(fordcardata)
#The total number of rows and columns can be verified using the below function
dim(fordcardata)
# The unique values of each column can be identified
rapply(fordcardata,function(x)length(unique(x)))
#verify that are there any null values in the dataset
sum(is.na(fordcardata))
colSums(is.na(fordcardata))
# verify the outliers using box plot
boxplot(fordcardata)
# IQR*1.5(IQR - Inter Quartile Range) Rule can be used to remove the outliers
# This rule can be applied to the non-factor variables which are "year", "price", "mileage", "tax", "mpg"
Q1_year <- quantile(fordcardata$year, .25)
Q3_year <- quantile(fordcardata$year, .75)
IQR_year <- IQR(fordcardata$year)
Q1_price <- quantile(fordcardata$price, .25)
Q3_price <- quantile(fordcardata$price, .75)
IQR_price <- IQR(fordcardata$price)
Q1_mileage <- quantile(fordcardata$mileage, .25)
Q3_mileage <- quantile(fordcardata$mileage, .75)
IQR_mileage <- IQR(fordcardata$mileage)
Q1_tax <- quantile(fordcardata$tax, .25)
Q3_tax <- quantile(fordcardata$tax, .75)
IQR_tax <- IQR(fordcardata$tax)
Q1_mpg <- quantile(fordcardata$mpg, .25)
Q3_mpg <- quantile(fordcardata$mpg, .75)
IQR_mpg <- IQR(fordcardata$mpg)
fordcardata1 <- subset(fordcardata, fordcardata$year > (Q1_year - 1.5*IQR_year) & fordcardata$year < (Q3_year + 1.5*IQR_year)
                       & fordcardata$price > (Q1_price - 1.5*IQR_price) & fordcardata$price < (Q3_price + 1.5*IQR_price)
                       & fordcardata$mileage > (Q1_mileage - 1.5*IQR_mileage) & fordcardata$mileage < (Q3_mileage + 1.5*IQR_mileage)
                       & fordcardata$tax > (Q1_tax - 1.5*IQR_tax) & fordcardata$tax < (Q3_tax + 1.5*IQR_tax)
                       & fordcardata$mpg > (Q1_mpg - 1.5*IQR_mpg) & fordcardata$mpg < (Q3_mpg + 1.5*IQR_mpg))
# verify the extreme outliers are removed
boxplot(fordcardata1)
# verify the count of the data
dim(fordcardata1)
# get the structure of the data
str(fordcardata1)
#libraries needed
library(caret)
library(class)
library(e1071)
install.packages("FNN")
library(FNN)
install.packages("gmodels")
library(gmodels) 

fordcar_data2 <- fordcardata1
library(dplyr)
# put outcome in its own object
transmission_outcome <- fordcar_data2 %>% select(transmission)

# remove original variable from the data set
fordcar_data2 <- fordcar_data2 %>% select(-transmission)
# get the column names
colnames(fordcar_data2)
# change year to categorical variable
fordcar_data2$year = as.factor(fordcar_data2$year)
str(fordcar_data2)
# scale the data in case if our features on different metrics as KNN relies on distances
# scaling to a z score metric as we are using scale function
fordcar_data2[,c("price", "mileage", "tax", "mpg", "engineSize")] <- scale(fordcar_data2[,c("price", "mileage", "tax", "mpg", "engineSize")])
# verify the scaling results, check the first few rows of data
head(fordcar_data2)
str(fordcar_data2)
# dummy code the variables which has more than 3 levels using the psych library
library(psych)
model <- as.data.frame(dummy.code(fordcar_data2$model))
year <- as.data.frame(dummy.code(fordcar_data2$year))
fuelType <- as.data.frame(dummy.code(fordcar_data2$fuelType))
# combine the dummy variables to our fordcar_data2 data
fordcar_data2 <- cbind(fordcar_data2, model, year, fuelType)
# check the structure of data
str(fordcar_data2)
# check the column names of data
colnames(fordcar_data2)
# remove model, year and fuel type variable as we created the dummy variable for this
fordcar_data2 <- fordcar_data2 %>% select(-one_of(c("model", "year", "fuelType")))
# verify the column names
colnames(fordcar_data2)
# check few rows of data
head(fordcar_data2)
# to check the correlation between variables
install.packages("ggcorrplot")
library(ggcorrplot)
corr <- round(cor(fordcar_data2), 10)
ggcorrplot(corr)
set.seed(1234)
# Partition 75% training set and 25% training set
Samplesize <- floor(0.75 * nrow(fordcar_data2))
Samplesize

# separately store the outcome variable and other variables
train_forddata <- sample(seq_len(nrow(fordcar_data2)), size = Samplesize)
train_forddata

# creating test and training sets that contain all of the independent variables
ford_pred_train <- fordcar_data2[train_forddata, ]
#to know the sample size
nrow(ford_pred_train)
#12021
ford_pred_test <- fordcar_data2[-train_forddata, ]
nrow(ford_pred_test)
# creating test and training sets that contain dependent variable
transmission_outcome_train <- transmission_outcome[train_forddata, ]
transmission_outcome_test <- transmission_outcome[-train_forddata, ]
# get the sq rt of sample size
sqrt(Samplesize)
#109  sample size - odd number
install.packages("class")
library(class)
# knn model using knn function
transmission_pred_knn <- knn(train = ford_pred_train, test = ford_pred_test, cl = transmission_outcome_train, k=109)
# put "transmission_outcome_test" in a data frame
transmission_outcome_test <- data.frame(transmission_outcome_test)
# to verify the accuracy, confusion matrix is used
#install.packages("caret")
library(caret)
confusionMatrix(transmission_pred_knn, transmission_outcome_test$transmission_outcome_test)

# merge "transmission_pred_knn" and "transmission_outcome_test" 
class_comparison <- data.frame(transmission_pred_knn, transmission_outcome_test)

# specify column names for "class_comparison"
names(class_comparison) <- c("PredictedTransmission", "ObservedTransmission")

# inspect "class_comparison"
head(class_comparison)


library(gmodels)
# creating table for examining model accuracy
CrossTable(x = class_comparison$ObservedTransmission, y = class_comparison$PredictedTransmission, prop.chisq=FALSE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE)


#the caret function picks the optimal number of neighbors (k)
transmission_pred_caret <- train(ford_pred_train, transmission_outcome_train, method = "knn", preProcess = c("center","scale"))
transmission_pred_caret
plot(transmission_pred_caret)
knnPredict <- predict(transmission_pred_caret, newdata = ford_pred_test)
knnPredict
# confusion matrix
caret::confusionMatrix(knnPredict, transmission_outcome_test$transmission_outcome_test)

