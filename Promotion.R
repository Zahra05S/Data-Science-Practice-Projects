# set the directory
setwd("C:/Users/HP/Desktop/NCI - MS Data Analytics/Semester 1/Data Mining and Machine learning 1/Project data set")
# get the directory in console
getwd()
# read the file and save in employeepromotion
employeepromotion <- read.csv("employee_promotion.csv", header=T, na.strings=c(""),stringsAsFactors = T)
# get the structure and some rows of data
head(employeepromotion)
# summary of the data includes descriptive statistics
summary(employeepromotion)
# structure of the data
str(employeepromotion)
#The total number of rows and columns can be verified using the below function
dim(employeepromotion)
# The unique values of each column can be identified
rapply(employeepromotion,function(x)length(unique(x)))
#verify that are there any null values in the dataset
colSums(is.na(employeepromotion))
# drop the rows which has null values using tidyr
library(tidyr)
employeepromotion1 <- drop_na(employeepromotion)
#The total number of rows and columns can be verified using the below function
dim(employeepromotion1)
#verify that are there any null values in the dataset
colSums(is.na(employeepromotion1))
# change the independent variable to factor
employeepromotion1$is_promoted = as.factor(employeepromotion1$is_promoted)
# structure of the data
str(employeepromotion1)
# verify the box plot
boxplot(employeepromotion1)
#pairs(employeepromotion1,panel=panel.smooth)
# split the data in to 80 % training and 20% testing set
library(caTools)
set.seed(4566)
split<-sample.split(employeepromotion1,SplitRatio = 0.8)
split
training<-subset(employeepromotion1,split=="TRUE")
testing<-subset(employeepromotion1,split=="FALSE")
# verify the number of split
nrow(training)
nrow(testing)
# logistic regression model using glm() function in training data
modellog<-glm(is_promoted~.,training,family="binomial")
summary(modellog)
# remove gender in model2 updating modellog as the variable is not significant
model2 <- update(modellog,~. -gender)
summary(model2)
# remove recruitment_channel in model3 updating model2 as the variable is not significant
model3 <- update(model2,~. -recruitment_channel)
summary(model3)
# remove length_of_service in model4 updating model3 as the variable is not significant
model4 <- update(model3,~. -length_of_service)
summary(model4)
# remove employee_id in model5 updating model4 as the variable is not significant
model5 <- update(model4,~. -employee_id)
summary(model5)
# predict the output using the model in our testing data
log_predict1 <- predict(model5,newdata = testing,type = "response")
# split the value if >0.5 then 1 or else 0
log_predict1 <- ifelse(log_predict1 > 0.5,1,0)
# using confusion matrix check the accuracy
caret::confusionMatrix(as.factor(log_predict1),as.factor(testing$is_promoted))

# check the threshold value using ROC curve
library(ROCR)
res1<-predict(model5,training,type="response")
rocrpred = prediction(res1, training$is_promoted)
rocrperf <- performance(rocrpred, "tpr", "fpr")
plot(rocrperf, colorsize = TRUE, print.cutoffs.at=seq(0.1,by=0.1))
# predict the output using the model in our testing data
log_predict <- predict(model5,newdata = testing,type = "response")
# split the value if >0.5 then 1 or else 0
log_predict <- ifelse(log_predict > 0.4,1,0)
# using confusion matrix check the accuracy
caret::confusionMatrix(as.factor(log_predict),as.factor(testing$is_promoted))

log_predict
# to check the AUC - ROC curve 
library(ROCR)
install.packages("Metrics")
library(Metrics)
pr <- prediction(log_predict,testing$is_promoted)
pr
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 
perf
plot(perf) > auc(testing$is_promoted,log_predict)


# predict the values using model5 in the test dataset
predict(model5, data = testing) -> Promotion_result
# The predicted result for count is in "count_result". Apply log for the count column in test data set 
#and bind it with "count_result" as below and store it in final_result
cbind(Actual = testing$is_promoted, predicted = log_predict) -> final_result
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



# Decision tree
install.packages("party")
library(party)
#decision tree model
# Ctree() creates splits iteratively for the variable which is 
#most significantly associated with the response variable measured using p values
model1 <- ctree(is_promoted~., data = training, controls = ctree_control(mincriterion = 0.95, minsplit =1500))
model1
#plot the decision tree model using ctree
plot(model1)
#predictions
# predicting the output using testing data using our trained model
promotion_model_predicted <- predict(model1, newdata=testing, type ='response')
promotion_model_predicted
# to view the result
# predict the values using model6 in the test dataset
predict(model1, newdata = testing) -> Promotion_result
# The predicted result for count is in "count_result". Apply log for the count column in test data set 
#and bind it with "count_result" as below and store it in final_result
cbind(Actual = testing$is_promoted, predicted = Promotion_result) -> final_result
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
#confusion matrix
caret::confusionMatrix(promotion_model_predicted, testing$is_promoted)


#building our model with tree() function 
install.packages("tree")
library(tree)
#"is_promoted" is a continuous variable, and it will be recode as binary variable. 
#Ifelse() function is used to create a variable "tree1" which takes the value "1" if the "is_promoted" is yes else it is no
tree1 <- ifelse(employeepromotion1$is_promoted == 1,"yes","no")
# change the tree1 into factor
tree1 <- as.factor(tree1)
# data.frame() function can be used to merge "tree1" with the rest of the data
employeepromotion1 <- data.frame(employeepromotion1,tree1)
#The tree() function to fit a classification tree in order to predict "tree1" using all variables 
#except "is_promoted", "employee_id" (it is removed because all the values are unique and 
#it will not contribute any to our prediction) and "region" (factor predictors must have 
#at most 32 levels, but region has 35 levels. Hence it is removed)
tree.employee_promotion <- tree(tree1 ~ . -is_promoted-employee_id-region, employeepromotion1)
# to view the summary
summary(tree.employee_promotion)
# plot the tree - tree structure can be displayed graphically and the 
#text() function is used to display the label of nodes. 
#The argument pretty=0 includes the category names for any qualitative predictors.
plot(tree.employee_promotion)
text(tree.employee_promotion , pretty = 0)
#The most important indicator of is_promoted appears to be avg_training_score. 
#Branches which leads to terminal nodes are indicated in asterisks
tree.employee_promotion
set.seed (2)
#split the dataset in to training and testing set. 
#75 percent in the training and remaining in the testing set.
traintree <- sample (1: nrow (employeepromotion1), 0.75 * nrow(employeepromotion1))
testtree <- employeepromotion1[-traintree , ]
tree.test <- tree1[-traintree]
tree.test
#tree() function is used to do the model using the traintree subset. 
#Predict() function can be used to predict the model fit using the testtree data subset, 
#and "type=class" returns the actual prediction.
# apply decision tree model using tree() function
tree.employee_promotion <- tree(tree1 ~ . -is_promoted-employee_id-region, employeepromotion1, subset=traintree)
tree.employee_promotion
#predict the output using the model
tree.pred <- predict(tree.employee_promotion , testtree ,type = "class")
tree.pred
table(tree.pred, tree.test)
# confusion matrix to predict accuracy
caret::confusionMatrix(tree.pred, tree.test)

#pruning
#The cv.tree() function provides the size which tells the number of terminal nodes considered, 
# corresponding error rate, and k value which is called cost complexity parameter
prunePromoted <- cv.tree(tree.employee_promotion, FUN = prune.misclass)
names(prunePromoted)
# shows the terminal nodes and cross validation errors
prunePromoted
#plot the size and k value
par (mfrow = c(1, 2))
plot (prunePromoted$size , prunePromoted$dev, type = "b")
plot (prunePromoted$k, prunePromoted$dev, type = "b")
# prune.missclass() function is applied to prune the tree with the size value 7
prune.promotion <- prune.misclass(tree.employee_promotion, best = 7)
plot(prune.promotion)
text(prune.promotion , pretty = 0)
# apply the pruned tree in the test dataset. Predict() function can be applied
tree.pred <- predict (prune.promotion, testtree, type = "class")
# table shows the count of yes or no values

table(tree.pred , tree.test)
# confusion matrix predict the accuracy and other measures
caret::confusionMatrix(tree.pred, tree.test)
