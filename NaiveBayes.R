# NaiveBayes algorithm is a supervised ML algorithm used for classification

install.packages("naivebayes")
install.packages("psych")

library(naivebayes)
library(psych)
library(dplyr)
library(tidyverse)

mydata <- read.csv("binary.csv", header = T)
View(mydata)
str(mydata)
glimpse(mydata)   # displays number of rows/columns, datatype and values in each column header in the dataset

xtabs(~ admit + rank, data = mydata)  # xtab()function means cross tabulation. so we do cross tabulation of admit and rank columns. similar to table()function
                      

table(mydata$admit, mydata$rank)   # similar to xtab()function above

mydata$rank <- as.factor(mydata$rank)   # Rank column should be categorical not int values so convert rank column to a factor so only unique/distinct values in it are displayed
mydata$admit <- as.factor(mydata$admit)  # admit column should be categorical not int values so convert admit column to a factor so only unique/distinct values in it are displayed


str(mydata)  # displays the new datatypes of the two columns

# creating visuals
pairs.panels(mydata[-1])   

mydata %>%
  ggplot(aes(admit, gre, fill = admit)) +
  geom_boxplot() +       # plots a boxplot chart
  ggtitle("Box Plot")   # gives title to the chart

mydata %>%
  ggplot(aes(gre, fill = admit)) +
  geom_density(alpha = 0.8, color = "black") +    # plots a density line chart. 
  ggtitle("Density Plot")   # gives title to the chart


# data partitioning
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T,   # sample(2)-- taking a sample of size 2, nrow(mydata)--number of row is same as dataset
              prob = c(0.8, 0.2))            # replace = T -- sampling with replacement, prob = c(0.8, 0.2) means training data = 70% and testing data = 30%. prob means probability

train <- mydata[ind == 1, ]   # all rows with values 1 go into training dataset. all columns are included
train

test  <- mydata[ind == 2, ]  # all rows with values 2 go into training dataset. all columns are included
test


# Naive Bayes model
mymodel <- naive_bayes(admit ~ ., data = train)  
mymodel
            # OR
# mymodel <- naive_bayes(admit ~ ., data = train, useKernel = T)   where useKernel = T will make use of Kernel based densities
# mymodel

train %>%
   filter(admit == "0") %>%          # dis is inside mymodel variable above. we just wanted to be sure
   summarise(mean(gre), sd(gre))

train %>%
  filter(admit == "1") %>%           # dis is inside mymodel variable above. we just wanted to be sure    
  summarise(mean(gre), sd(gre))

plot(mymodel)   # dis displays 3 diff charts with each column values for each chart in Plots tab(use the backward arrow icon to see each chart one by one). 
               # 1st column does not have a chart becuz its just 2 distinct/unique values in it


# Prediction
mypred <- predict(mymodel, train, type = "prob")
head(mypred)

head(cbind(mypred, train))    # using cbind()function to add the 2 new created columns of mypred variable to main dataset and displaying a subset of it all using head()function
                           # shows the chances of admit column in %

# Confusion matrix for train dataset. Confusion matrix is used to display the result numbers of how the prediction varies with the reference of the dataset 
ourpred <- predict(mymodel, train)
tab1 <- table(ourpred, train$admit)
tab1

# misclassification
1 - sum(diag(tab1)) / sum(tab1)


# Confusion matrix for test dataset. Confusion matrix is used to display the result numbers of how the prediction varies with the reference of the dataset
ourpred2 <- predict(mymodel, test)
tab2 <- table(ourpred2, test$admit)
tab2

# misclassification
1 - sum(diag(tab2)) / sum(tab2)







