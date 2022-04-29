# Fast and Frugal Decision Trees model is used for only binary classification and prediction problems.
# Fast and Frugal Decision Trees model computes d Decision Tree diagram, Confusion Matrix, Performance Metric and ROC Curve all in one plot/chart, that is why it is diff from other Decision Tree algorithm


# creating a decision tree model to decide whether to buy or sell Apple stock
library(devtools)
install_github("ndphillips/FFTrees", type = "source", dependencies = TRUE)

library(FFTrees)

mydata <- read.csv(file.choose(), header = T)   # select Apple.csv
str(mydata)     # buy column is d response/dependent variable where value 0- sell Apple stocks, value 1- buy Apple stocks 
mydata$buy <- as.logical(mydata$buy)  # changes buy column to logical datatype. Make sure d response/dependent variable is in logical datatype(ie its values are TRUE and FALSE) 
str(mydata)
names(mydata)

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = TRUE,
              prob = c(0.8, 0.2))
training <- mydata[ind == 1,]
testing <- mydata[ind == 2,]


# Tree Model
?FFTrees

mytree <- FFTrees(formula = buy ~., data = training,
                  data.test = testing,
                  main = "Apple Stock Buying Decisions",
                  decision.labels = c("Sell", "Buy"))   
mytree    

inwords(mytree)   # It explains d decision tree in words. it explains how d key columns and its values facilitates a buy or sell action. 

# summary(mytree)  # displays info about each tree it has generated under tree column. it generated 6 trees(you see it in tree column)

names(mytree)  # displays names of columns created by d model- trees, cues etc 

mytree$trees

mytree$cues

# mytree$auc   # displays area under curve values for training and testing dataset

# mytree$decision  # displays d final classification decisions. Each row is a case and each column is a tree

# cbind(training, mytree$decision$training)  # displays all d columns along with d dependent column and all d trees it generated as column-headers and d decision it arrives at for each tree- FALSE means 0, TRUE means 1



# Plot tree. since d model using training and testing data generated 6 trees, as such d training data will have 6 trees and d testing data will have 6 trees as well.
plot(mytree)   

plot(mytree, tree = 6)  # this displays d 6th tree in training data- FFT #6(of 6) and its Accuracy(Training)


# visualize d chart using testing data
plot(mytree, data = "test") 

plot(mytree, data = "test", tree = 6)  

plot(mytree, what = "cues")   

plot(mytree, stats = F)  # displays d decision tree model only


# Predict using testing data
mypred <- predict(mytree, testing)
head(mypred)      # compare d predicted values(for buy column) in head(mypred) to d actual values in head(mydata$buy)
head(mydata$buy)


# using my.tree parameter to add conditional statements from some columns. my.tree parameter reps an FFT in words 
ourtree <- FFTrees(formula = buy ~., data = training,
                     data.test = testing,
                     main = "Apple Stock Buying Decisions",
                     decision.labels = c("Sell", "Buy"),   
                     my.tree = "If KDJ > 0.8, predict Sell.
                                If fastK > 0.8, predict Sell Otherwise, predict Buy")  # KDJ and fastK are independent columns used in d model
ourtree   # generates only 1 tree                         


plot(ourtree, data = "test")  # using testing data to display d one tree generated



# using FFForest(which is a forest of fast and frugal decision trees) to obtain Cue Importance

myforest <- FFForest(buy ~., data = mydata,
                     ntree = 50, train.p = 0.5)  # train.p = 0.5 means we use 50% of data to fit each tree(ie we use 50% of d datapoints wheneva a tree is developed in d random forest model)
myforest

plot(myforest)  #
