# using caret package for Classification and Regression to create Linear Regression, Ridge Regression, Lasso Regression and Elastic Net Regression models 

# Collinearity lead to overfitting so to solve dis problem, we create:
# 1- Ridge regression; dis shrinks coefficients to non-zero values to prevent overfitting but keeps all variables
# 2- Lasso Regression; dis shrinks regression coefficient, with some shrunk to zero. It helps with feature selection
# 3- Elastic Net Regression: This is a mix/combination of Ridge and Lasso Regression 

# Libraries Needed
library(caret)
library(glmnet)
library(mlbench)
library(psych)

# Data
data("BostonHousing")
mydata <- BostonHousing   # medv is dependent column
str(mydata)

# Lets display correlation values for all numeric variables/columns only
pairs.panels(mydata[c(-4,-14)])  # removing d 4th column becuz it is a factor variable and also removing d dependent variable(14th column)
pairs.panels(mydata[c(-4,-14)], cex.cor = 2)  # cex.cor = 2 will increase d size of d correlation values 

# Data Partition
set.seed(222)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.7, 0.3))

train <- mydata[ind==1,]
test <- mydata[ind==2,]


# Creating Custom Control Parameters and Cross-validation
mycustom <- trainControl(method = "repeatedcv", number = 10,  # 10 fold cross validation means d training data is broken into 10 parts, den d model is made from 9 parts while d 10th part is used for error estimation. 
                       repeats = 5, verboseIter = T)   # Then d model is made again from 9 parts while d 9th part is used for error estimation. Again d model is made again from 9 parts while d 8th part is used for error estimation and so on...thus creating 10 models with diff parts used for error estimation   


#---- creating Linear Regression Model -----
set.seed(1234)

mymodel <- train(medv ~., train, method = "lm", trControl = mycustom)  # medv is d dependent column
mymodel

mymodel$results  # RMSE- Root Mean Square Error, RMSESD- Root Mean Square Error Standard Deviation, MAESD- Mean Absolute Error Standard Deviation
summary(mymodel)

plot(mymodel$finalModel)  # Press Enter in d Console tab so the plot is displayed one by one in Plots tab



#--- Creating Ridge Regression -----
# This shrinks d coefficients to non-zero values to prevent overfitting but keeps all independent variables
set.seed(1234)

myridge <- train(medv ~., train, method = "glmnet", 
                 tuneGrid = expand.grid(alpha = 0,
                                        lambda = seq(0.0001, 1, length = 5)),  # dis creates a sequence of 5 values btw 0.0001 and 1
                 trControl = mycustom)  
myridge

plot(myridge)  # d x-axis values are lambda values and we see that d optimal value of lambda is 0.5
plot(myridge$finalModel, xvar = "lambda", label = T)  # d values at d top of d plot is d number of independent variables/columns used
plot(myridge$finalModel, xvar = 'dev', label = T)

plot(varImp(myridge, scale = T))  # displays variable importance



#---- Creating Lasso(Least absolute shrinkage and selector operator) Regression ------
# This shrinks d regression coefficient, with some shrunk to zero. It also does feature selection
set.seed(1234)

mylasso <- train(medv ~., train, method = "glmnet", 
                 tuneGrid = expand.grid(alpha = 1,
                                        lambda = seq(0.0001, 1, length = 5)),  
                 trControl = mycustom)
mylasso

plot(mylasso)


mylasso <- train(medv ~., train, method = "glmnet", 
                 tuneGrid = expand.grid(alpha = 1,
                                        lambda = seq(0.0001, 0.2, length = 5)),  
                 trControl = mycustom)
mylasso

plot(mylasso)
plot(mylasso$finalModel, xvar = 'lambda', label = T)
plot(mylasso$finalModel, xvar = 'dev', label = T)

plot(varImp(mylasso, scale = T))  # displays variable importance



#---- Creating Elastic Net Regression -----
# This is a mix/combination of Ridge and Lasso Regression
set.seed(1234)

mynet <- train(medv ~., train, method = "glmnet", 
               tuneGrid = expand.grid(alpha = seq(0, 1, length = 10),  # displays a set of 10 values btw 0 and 1
                                      lambda = seq(0.0001, 1, length = 5)),  
               trControl = mycustom)
mynet

plot(mynet)



mynet <- train(medv ~., train, method = "glmnet", 
               tuneGrid = expand.grid(alpha = seq(0, 1, length = 10),  
                                      lambda = seq(0.0001, 0.2, length = 5)),  
               trControl = mycustom)
mynet

plot(mynet)
plot(mynet$finalModel, xvar = 'lambda', label = T)
plot(mynet$finalModel, xvar = 'dev', label = T)

plot(varImp(mynet))


# Lets compare all the Models created
ourmodel <- list(LinearModel = mymodel, RidgeModel = myridge, 
                 LassoModel = mylasso, ElasticNetModel = mynet)

res <- resamples(ourmodel)  # resamples() is in caret package
summary(res)

bwplot(res)  # displays a boxplot chart using d above summary values

xyplot(res, metric = "RMSE")  # displays scatterplot with d dots along d diagonal line


# Getting our Best Model on Elastic Net model
mynet$bestTune

best <- mynet$finalModel
coef(best, s = mynet$bestTune$lambda)  # we see which column has d highest negative and positive values


# Save the Final Model for later use. You need to set yr working directory
saveRDS(mynet, "final_model.rds")  # dis saves d best model as an .RDS file

# fm <- readRDS("final_model.rds")  # after shutting down d software, use dis to read-in d model .RDS file after running d libraries, the dataset and the Partitioning of d dataset all above. u dont need to run any model
# print(fm)


# Making Prediction on training data
p1 <- predict(mynet, train)   # den replace mynet with fm

sqrt(mean((train$medv)^2))  # displays d RMSE value for d training data
mean((train$medv - p1)^2)   # displays d MSE value for d training data
sqrt(mean((train$medv - p1)^2))  # displays d RMSE value for d training data


# Making Prediction on testing data
p2 <- predict(mynet, test)     # den replace mynet with fm

sqrt(mean((test$medv) ^ 2))  # displays d RMSE value for d testing data
mean((test$medv - p2) ^ 2)    # displays d MSE value for d testing data
sqrt(mean((test$medv - p2) ^ 2))  # displays d RMSE value for d testing data
