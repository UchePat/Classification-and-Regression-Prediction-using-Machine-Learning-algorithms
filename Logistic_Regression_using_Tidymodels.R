# LIBRARIES ----
install.packages("tidymodels")
install.packages("vip")

library(tidymodels)   # dis helps with d logistic regression
library(vip)    # vip - variable importance. how important a variable/column is to d model
library(tidyverse)

# 1.0 DATA ----
View(mpg)

# 2.0 DATA WRANGLING ----
mydata <- mpg %>%
  select(displ:class) %>%     # using only from displ column to class column
  mutate(trans = ifelse(str_detect(trans, "auto"), "auto", "manual")) %>%   # means if value- auto is detected in trans column write auto, else write manual. this helps to remove d l5 in auto value and m5 in manual value value
  relocate(year, .before = 1) %>%  # since displ column is d 1st column(remember we are using from displ column to class column), we use relocate() to move year column before displ column making it d 1st column 
  mutate(year = as.factor(year))   # make year column a factor variable

mydata


# 3.0 TRAIN / TEST SPLITS ----
set.seed(123)
splits <- mydata %>%
  initial_split(prop = 0.80)   # training data is 80%, testing data into 20%

splits        # <187/47/234> means 187 rows of values is for Analysis/training data, 47 rows of values is for Assess/testing data, while d dataset has a total of 234 rows of values


# 4.0 Logistic Regression Model ----
mymodel <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(year ~ . , data = training(splits))   # training(splits) is training data. year column is our dependent variable

mymodel


# 5.0 PREDICTION of Testing data----
mypred <- predict(mymodel, new_data = testing(splits), type = "class")   # testing(splits) is testing data
head(mypred)
head(testing(splits))   # we compare d predicted values in head(mypred) and d actual values in our training data- head(testing(splits)) 

mypred1  <- predict(mymodel, new_data = testing(splits), type = "prob")
head(mypred1)   # displays probability values for each index to be either 1999 or 2008(ie index 1 is 1999 since column 1999 has a higher prob value than 2008, index 2 is 2008 ...)

myresults <- bind_cols(mypred, mypred1,
                       testing(splits))  # joins d predicted columns- mypred and mypred1 to d testing dataset. bind_cols() is similar to column bind function 
myresults


# 6.0 EVALUATION: AUC(Area Under the Curve)
myresults %>%
  roc_auc(year, .pred_1999)  # displays an AUC value- 0.839 using d columns stated

myresults %>%
  roc_curve(year, .pred_1999) %>%
  autoplot(options = list(smooth = TRUE)) +
  labs(title = "Area Under the Curve (AUC): 0.839")   # creates an AUC curve. 
# d AUC curve should be above d dotted straight line for our model to be legit/significant. if d AUC curve is at d dotted line or below d dotted line den our model is flawed


# 7.0 FEATURE IMPORTANCE ----
# * Visualize Most Important Features/Variables ----
mymodel$fit %>%
  vip(num_features = 20, geom = "point",
      aesthetics = list(size = 4, color = "#18bc9c")) +
  theme_minimal(base_size = 18) +
  labs(title = "Logistic Regression: Feature Importance")   # we see which column is most significant to d model, hwy column is most significant to d model


# * Visualize d 2 Top Features/Variables ----
mydata %>%
  ggplot(aes(class, hwy, color = year)) +   # we use d two most important columns to d model- hwy and class columns
  geom_boxplot() +               # since hwy column is numerical and class column is categorical, we use boxplot. displays multiple boxplot
  geom_jitter(alpha = 0.25) +
  theme_minimal(base_size = 18) +
  scale_color_viridis_d(end = 0.4) +
  labs(title = "Older Vehicles have Lower Fuel Economy")
