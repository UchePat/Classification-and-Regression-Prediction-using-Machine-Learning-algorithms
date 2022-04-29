
# Correlation is d measure of d relationship of dependency amongst the variables of a dataset
# With Correlation, we analyze d relationship that d independent variables of d dataset possess

mydata <- read.csv("bank-loan.csv", header = TRUE,
                   stringsAsFactors = FALSE)
str(mydata)

ourdata <- mydata[, c(1, 3, 4, 5, 6, 7, 8)]  # dese are d independent columns/variables
head(ourdata)

# Creating d Correlation Analysis
mycor <- cor(ourdata)  # creates matrix of d columns displaying d correlation values btw dem 
mycor

# Visualize d Correlation Analysis
library(corrplot)

corrplot(mycor, method = "circle")

corrplot(mycor, method = "number")
