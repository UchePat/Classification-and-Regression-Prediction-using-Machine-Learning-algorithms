# Statistical Inference is done using either Frequentist approach (which is Linear Regression model) OR Bayesian approach (which is Bayesian Regression model)
# Here we are comparing Frequentist approach (which is Linear Regression model) and Bayesian approach (which is Bayesian Regression model)

suppressPackageStartupMessages(library(mlbench))  # dis will suppress/hide d warning/msg that displays when you load a library
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(insight))

data("BostonHousing")    # dis dataset is in mlbench package
str(BostonHousing)

# we are using only 3 main independent variables - age, dis and chas columns alongside d dependent variable - medv
mydata <- BostonHousing[, c("medv", "age", "dis", "chas")]
head(mydata)
str(mydata)
is.na(mydata)  # there are no missing values


#--- Creating Linear Regression model (Frequentist approach)---
mymodel <- lm(medv ~., data = mydata)
tidy(mymodel)
# we can see that dis column/variable is insignificant to d model becus its p.value is > 0.05


#--- Creating Bayesian Regressian model (Bayesian approach) using stan_glm() from rstanarm package---
bmodel <- stan_glm(medv ~., data = mydata, seed = 111)
print(bmodel , digits = 3)
# Median value is d median computed from the MCMC simulation(which is d default estimating approach algorithm used in creating d simulation). 
# MAD_SD is the median sd computed from d same simulation. Lets plot the MCMC simulation of each predictor/independent column used to understand the outputs displayed here.


# Plotting the MCMC of each predictor column used using bayesplot package
mcmc_dens(bmodel, pars = c("age")) +  # we can see that d point estimate(the red line) of age column falls on d median of dis distribution
  vline_at(-0.143, col = "red")  # the value used in vline parameter is same value for age under Median column in d Bayesian model created earlier(in print(bmodel , digits = 3))

mcmc_dens(bmodel, pars = c("dis")) +  # we can see that d point estimate(the red line) of dis column falls on d median of dis distribution
  vline_at(-0.244, col = "red")   # the value used in vline parameter is same value for dis under Median column in d Bayesian model created earlier(in print(bmodel , digits = 3))

mcmc_dens(bmodel, pars = c("chas1")) +
  vline_at(7.496, col = "red")


# Evaluating the Bayesian model parameters using describe_posterior()
describe_posterior(bmodel)
# Any variable with +ve and -ve values in 90% CI is non-significant as such dis variable is insignificant to d model (age has only -ve values, chas1 has only +ve values). Also we can see that dis variable has very low pd value 
# 90% CI - 90% Credible Interval - used to quantify d uncertainty about d regression coefficients
# pd - Probability of Direction. This is equivalent to p-value. it is d probability that d coefficient goes to d positive direction or negative direction
# rope - Region of Practical Equivalence(ROPE). It values ranges from -0.1 to 0.1 from d standardized coefficients
# Rhat - Scale reduction factor R. when dis values is close to 1, we do not have a convergence problem with MCMC
# ESS - effective sample size. the higher the value the better


# We can get the coefficient estimates/median by using insights package
post <- get_parameters(bmodel)
print(purrr::map_dbl(post, median), digits = 3)  # displays same median values in describe_posterior(bmodel) above

# We can compute the Maximum A posteriori (MAP) and the mean 
print(purrr::map_dbl(post, map_estimate), digits = 3)

print(purrr::map_dbl(post, mean), digits = 3)
# we can see that all d values in d 3 prints above are closer to each other due to d normality of d distribution of the posteriors where 
# all d central statistics(mean, median and mode) are closer to each other


# Lets Visualize the age column coefficients
mcmc_dens(bmodel, pars = c("age")) +
  vline_at(median(post$age), col = "red") +
  vline_at(mean(post$age), col = "yellow") +
  vline_at(map_estimate(post$age), col = "green")


#---- Bayesian Inference: Testing the significance of bayesian regression coefficient---
# Lets check the significance of each coefficient using;
hdi(bmodel)   # hdi - Highest Density Interval. this is a method to compute CI(Credible Interval). any variable with +ve and -ve values in 90% HDI is non-significant as such dis variable is non-significant (age has only -ve values, chas1 has only +ve values)  
# dis shows that dis variable is insignificant to d model just as it was predicted in Linear Regression model much much earlier

eti(bmodel)  # eti - Equal-tailed Interval. this is a method to compute CI(Credible Interval). any variable with +ve and -ve values in 90% ETI is non-significant as such dis variable is non-significant (age has only -ve values, chas1 has only +ve values) 
# dis shows that dis variable is insignificant to d model just as it was predicted in Linear Regression model much much earlier


# We can also Test the Significance by checking d part of the Credible Interval that falls inside the ROPE interval using rope()
# lets see the proportion of samples in each column/coefficient that is inside the ROPE range
rope(post$age)  # rope - Region of Practical Equivalence(ROPE). It is a small range around 0 [-0.1, 0.1]  from d standardized coefficients
# since it is 0.00%, it means all the Credible Interval(HDI) is outside the ROPE range which means that d coefficient is highly significant

rope(post$chas1)
# since it is 0.00%, it means all the Credible Interval(HDI) is outside the ROPE range which means that d coefficient is highly significant

rope(post$`(Intercept)`)
# since it is 0.00%, it means all the Credible Interval(HDI) is outside the ROPE range which means that d coefficient is highly significant

rope(post$dis)
# 18.76% is inside the ROPE range/interval meaning d probability of dis coefficient to be zero is 18.76%

rope_range(bmodel)


# PD (Probability of Direction. This is equivalent to p-value.) and P-value
# high value means dat the associated effect is concentrated on the same side as the median.
# for our bayesian model in describe_posterior(bmodel), since pd values equal to 1, almost all posteriors of d age and chas1 variables
# and Intercept are on the same side.
yrdata <- select(tidy(mymodel), c(term, p.value))  # using Linear model created earlier

yrdata$p.value <- round(yrdata$p.value, digits = 3)

yrdata2 <- 1 - purrr::map_dbl(post, p_direction)

yrdata3 <- cbind(yrdata, yrdata2)
yrdata3




