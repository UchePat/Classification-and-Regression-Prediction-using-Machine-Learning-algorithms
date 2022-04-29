
# Classification and Regression using ML algorithms in H2O AutoML package on Titanic dataset

require(h2o)

h2o.init()

train <- read.csv("train.csv")
test <- read.csv("test.csv")
str(train)

colSums(is.na(train))   # to check how many null values are in each column 


# change any column/variable that looks like it should be a categorical/factor variable to factor datatype especially d dependent variable 
train$Survived <- factor(train$Survived)
train$Pclass <- factor(train$Pclass)
train$Embarked <- factor(train$Embarked)
str(train)

test$Pclass <- factor(test$Pclass)
test$Embarked <- factor(test$Embarked)
str(test)


# convert d train and test dataset to h20 frame. You need to be online to run d code below 
mytrain <- as.h2o(na.exclude(train[,-c(1,3,4,6,9,11)]))  

mytest <- as.h2o(na.exclude(test[,-c(1,3,5,8,10)]))



#---- Creating ML models using H2O package -------

# creating Logistical Regression(GLM) model 
?h2o.automl
mymodel <- h2o.automl(y = "Survived", training = mytrain, max_runtime_secs = 36000,
                      stopping_metric = "misclassification", nfolds = 5,   
                      include_algos = c("GLM"))   
                                     
mymodel@leaderboard

mymodel@leader


# Prediction on test data
mypred <- h2o.predict(mymodel@leader, mytest)  # using d model in making prediction

(mypred)$predict  # Displays d predicted result for Survived column in train data using mytest data. NOTE: mytest/test objects have no Survived column  
head(train$Survived)  # compare d predict result in (mypred)$predict object and d actual result for Survived column in head(train$Survived)


#---------------------------------------------------------------------------------------------


# creating Logistical Regression(GLM) and Gradient Boosting(GBM) models together. this will take some time to execute
mymodel <- h2o.automl(y = "Survived", training = mytrain, max_runtime_secs = 36000,
                      stopping_metric = "misclassification", nfolds = 5,
                      include_algos = c("GLM", "GBM"))

mymodel@leaderboard

mymodel@leader   # displays d best model among d two ML algorithms used


# Prediction on test data
mypred <- h2o.predict(mymodel@leader, mytest)  # using d best model in making prediction

(mypred)$predict



#---------------------------------------------------------------------------------------------


# creating multiple ML models. this will take a lot of time to execute
mymodel <- h2.automl(y = "Survived", training = mytrain, max_runtime_secs = 36000,
                      stopping_metric = "misclassification", nfolds = 5)  
                      
mymodel@leaderboard

mymodel@leader    # displays d best model among all d ML algorithms used



# Prediction on test data
mypred <- h2o.predict(mymodel@leader, mytest)  # using d best model in making prediction

(mypred)$predict




