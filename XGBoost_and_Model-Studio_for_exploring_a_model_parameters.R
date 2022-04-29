# Model-Studio package: Interactive Studio for Explanatory Model Analysis  ----
# Model-Studio package is used for exploring interactive models

# We are using XGBoost ML algorithm to create a Regression model(ie predicting a dependent variable ind dataset)
# and using modelstudio package to explore d interactive plots/parameters made from d model

# LIBRARIES ----
install.packages("modelStudio")
install.packages("DALEX")


library(modelStudio)
library(DALEX)
library(tidyverse)
library(tidymodels)

# DATA ----
mpg

mydata <- mpg %>%
  select(hwy, manufacturer:drv, fl, class)  # dis are d columns we are creating a Regression(prediction) model with
                                       # hwy is d dependent variable we are predicting
mydata


# Creating an XGBoost MODEL using tidymodel package----
mymodel <- boost_tree(learn_rate = 0.3) %>%
  set_mode("regression") %>%   # we are doing regression/prediction
  set_engine("xgboost") %>%
  fit(hwy ~ ., data = mydata)  # hwy is d dependent variable we are predicting

mymodel


# EXPLAINER ----
# this computes explanatory information from d model
explainer <- DALEX::explain(model = mymodel,
                            data = mydata, 
                            y = mydata$hwy, 
                            label = "XGBoost")


# MODEL STUDIO ----
modelStudio::modelStudio(explainer)
# displays a separate window tab showing various exploratory interactive plots from d model
# we see a chart showing Feature Importance(ie which column is more/less important to d model)
# The 1st dropdown at top RHS displays d column-names of all d independent columns used in d model, while d 2nd dropdown shows d ID and value of d dependent column
# You can click on any tab to display parameters and click any parameters to explore them(ie click on one empty tab and select a parameter to explore. Click on another empty tab and select another parameter to explore etc..)
# Breakdown Plot parameter and dis shows d positive/negative attribution each feature/column(and their value) has to d prediction(d last bar and value is d prediction). 
# Shapeley Values parameter shows d positive/negative importance of d columns(and their values)
# You can change d value in d dropdown in top RHS of d window tab(y is d dependent column)
# Partial Dependence parameter shows how d model prediction changes by varying d independent variable so u can change d dropdown value that contains d independent column-names in top RHS
# you can click d X icon in any parameter chart to exit and click d now empty tab to display d parameters again and select anoda parameter to explore
# You can hover over D icon to see more explanatory text about that parameter chart