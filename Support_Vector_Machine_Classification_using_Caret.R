# using CARET package to build an S.V.M model for classification

# Importing libraries
library(datasets) # Contains the Iris data set
library(caret) # Package for machine learning algorithms / CARET stands for Classification And REgression Training

# Importing the Iris data set
data(iris)

# Check to see if there are missing data?
sum(is.na(iris))


# Performs stratified random split of the data set 
set.seed(100)  # To achieve reproducible model; set the random seed number

ind <- createDataPartition(iris$Species, p = 0.8, list = FALSE)  # Species column is dependent column 

TrainingSet <- iris[ind,] # Training Set
TestingSet <- iris[-ind,] # Test Set


# Compare scatter plot of the 80 and 20 data subsets





#----- Building an S.V.M model (using polynomial kernel) -----
mymodel <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",   # svmPoly means Support vector Machine using polynomial kernel
               na.action = na.omit,  # removes missing value
               preProcess=c("scale","center"),     # this will normalize/standardize any numerical column
               trControl= trainControl(method ="none"),    # we are not using cross validation
               tuneGrid = data.frame(degree = 1, scale = 1, C = 1))
mymodel
summary(mymodel)


# Create Prediction using Training data
mypred <-predict(mymodel, TrainingSet) 

# Create Prediction on Testing set
mypred2 <-predict(mymodel, TestingSet) 


# Model performance (Displays confusion matrix and statistics)
a <-confusionMatrix(mypred, TrainingSet$Species)
a

b <-confusionMatrix(mypred2, TestingSet$Species)
b


# Feature/Variable importance
Importance <- varImp(mymodel)

plot(Importance)
plot(Importance, col = "red")




#-------------------------------------------------------------------------------------



#---- Another method of using caret package to create S.V.M model --------.
# lets use trainControl() function which enables parameter coefficient estimation using resampling approaches such as cross-validation and boosting
yrmodel <- trainControl(method ="cv", number = 10)   # creating 10 fold cross validation

# Build S.V.M model
mycv <- train(Species ~ ., data = TrainingSet,
              method = "svmPoly",
              na.action = na.omit,
              preProcess = c("scale","center"),
              trControl = yrmodel,
              tuneGrid = data.frame(degree = 1, scale = 1, C = 1))
mycv
summary(mycv)


# Create Prediction using Training data
mypred3 <-predict(mycv, TrainingSet) 


# Model performance (Displays confusion matrix and statistics)
c <-confusionMatrix(mypred3, TrainingSet$Species)
c
