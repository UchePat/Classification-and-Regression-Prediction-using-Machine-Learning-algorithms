
mydata <- read.csv("Cardiotocographic.csv")
str(mydata)
mydata$NSP <- as.factor(mydata$NSP)  # in NSP column(which is dependent/response variable), value 1 means Normal patient, value 2 means Suspect patient, value 3 means Pathologic patient
str(mydata)

set.seed(222)
ind <- sample(2, nrow(mydata), replace = TRUE,
              prob = c(0.6, 0.4))
training <- mydata[ind == 1,]
testing <- mydata[ind == 2,]

# Multinomial Logistic Regression model
library(nnet)   # nnet means neural net

training$NSP <- relevel(training$NSP, ref = "1")    # using any value in dependent variable(lets use value 1 in NSP column) as d reference level from d training dataset. Reference level needs to be done before modeling

mymodel <- multinom(NSP ~., data = training)  # multinom means Multinomial
mymodel      # the inital error values are high(all d values are called error values) and den they start reducing till they become constant(values become the same)
summary(mymodel)

# using 2-tailed z-test to find out which independent columns/variables are statistically significant to the model
z <- summary(mymodel)$coefficients / summary(mymodel)$standard.errors   # dividing coefficients values by standard.errors values of each independent column in mymodel object

p <- (1 - pnorm(abs(z), 0, 1)) * 2  # dis calculate p-values. pnorm means p-normal distribution, abs- absolute value, mean = 0, sd = 1. multiplying by 2 becuz it is a two-tailed test
p        # we check all columns values to find d column that either 1 or both of its values is < 0.05 (since p-value should be < 0.05 for dat column is significant). Any column that its values are both > 0.05 should be removed from the model.leave any column dat has both of its values or one of its values < 0.05


# removing columns that there 2 values are > 0.05 from the model and re-run the model
mymodel <- multinom(NSP ~.-MLTV -Width -Min -Max -Nmax -Nzeros -Tendency, data = training)  # multinom means Multinomial
mymodel      # the inital error values are high(all d values are called error values) and den they start reducing till they become constant(values become the same)
summary(mymodel)   # under Coefficients: we can use the 1st values of all the columns(including Intercept) to create the regression equation = -16.62047-0.07164775*LB-748.85498*AC+14.23592*FM.......... 
                  # we can also use the 2nd values of all the columns(including Intercept) to create a 2nd regression equation = -18.55244-0.40854245*LB-29.62735*AC+17.18672*FM.......... 



# using 2-tailed z-test to find out which independent columns/variables are statistically significant to the model
z <- summary(mymodel)$coefficients / summary(mymodel)$standard.errors   # dividing coefficients values by standard.errors values of each independent column in mymodel object

p <- (1 - pnorm(abs(z), 0, 1)) * 2  # dis calculate p-values. pnorm means p-normal distribution, abs- absolute value, mean = 0, sd = 1. multiplying by 2 becuz it is a two-tailed test
p        


# Confusion Matrix and Misclassification Error for Training data
mypred <- predict(mymodel, training)
head(mypred)         # we compare values in head(mypred) which is the predicted model and head(training$NSP) which is actual data for similarity and diff
head(training$NSP)

tab <- table(Predicted = mypred, Actual = training$NSP)  # values in diagonal are correct classifications while any oda values are misclassification
tab

# Accuracy
sum(diag(tab)) / sum(tab)

1- sum(diag(tab)) / sum(tab)

tab/colSums(tab)  



# Confusion Matrix and Misclassification Error for Testing data
mypred1 <- predict(mymodel, testing)
head(mypred1)         # we compare values in head(mypred) which is the predicted model and head(training$NSP) which is actual data for similarity and diff
head(testing$NSP)

tab1 <- table(Predicted = mypred1, Actual = testing$NSP)  # values in diagonal are correct classifications while any oda values are misclassification
tab1

# Accuracy
sum(diag(tab1)) / sum(tab1)

1- sum(diag(tab1)) / sum(tab1)


# Prediction and Model Assessment
ourdata <- table(training$NSP)  # displays total number of values in d dataset that corresponds to each distinct values of d dependent column
ourdata / sum(ourdata)  # displays d.p/% of total number of values in d dataset that corresponds to each distinct values of d dependent column

tab1/colSums(tab1)  





