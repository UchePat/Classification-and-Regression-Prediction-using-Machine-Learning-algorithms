# Creating Multiple Linear Regression Model using Caret package

# Importing libraries
library(mlbench) # Contains several benchmark data sets (especially the Boston Housing dataset)
library(caret) # Package for machine learning algorithms / CARET stands for Classification And REgression Training

# Importing the Boston Housing data set
data(BostonHousing)

mydata <- BostonHousing

head(mydata)   # medv column is dependent column

# Check to see if there are missing data?
sum(is.na(mydata))


# Performs stratified random split of the data set
set.seed(100)     # To achieve reproducible model; set the random seed number

ind <- createDataPartition(mydata$medv, p = 0.8, list = FALSE)

TrainingSet <- mydata[ind,] # Training Set
TestingSet <- mydata[-ind,] # Test Set


# Build Linear Regression model using training dataset
mymodel <- train(medv ~ ., data = TrainingSet,
               method = "lm",
               na.action = na.omit,
               preProcess=c("scale","center"),     # dis will standardize/normalize any column with numeric values
               trControl= trainControl(method ="none"))  # no cross validation
mymodel
summary(mymodel)  # Intercept means y-intercept

# Create Prediction on Training set
mypred <-predict(mymodel, TrainingSet) 

# Create Prediction on Testing set
mypred2 <-predict(mymodel, TestingSet) 


# Model performance (Displays scatter plot and performance metrics)
# comparing d Predicted values and Actual values of training data
plot(TrainingSet$medv, mypred, col = "blue")   # Scatter plot of Training set. The scatterplots should be in a diagonal line

# comparing d Predicted values and Actual values of testing data
plot(TestingSet$medv, mypred2, col = "blue")    # The scatterplots/dots should be in a diagonal line


# Lets calculate the correlation coefficient
mycor <- cor(TrainingSet$medv, mypred) 
mycor

mycor1 <- cor(TestingSet$medv, mypred2)
mycor1

# Calculate R-squared value
cor(TrainingSet$medv, mypred)^2

cor(TestingSet$medv, mypred2)^2
