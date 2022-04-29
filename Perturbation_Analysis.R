# Perturbation analysis is used to evaluate collinearity by adding random noise to selected variables/columns
# Perturbation analysis can be used for all regression-like models

# data
library(car)

View(Duncan)   # dis dataset is in car package
head(Duncan)

str(Duncan)   # we can see dat type column is a factor with categorical values. male sure d column with categorical values is of factor datatype 
summary(Duncan)

pairs(Duncan[2:4])  


# Creating Linear Regression model and VIF(Variance Inflation Factor) which is used to check for multicollinearity problem
# creating a linear model using only numerical independent variables/columns
mymodel <- lm(prestige ~ income + education, Duncan)  # prestige column is d response/dependent variable
summary(mymodel)

anova(mymodel)

vif(mymodel)   # since d values in d 2 columns are < 10, there is no multicollinearity problem


# Perturbation Analysis with numerical independent variables
# Perturbation Analysis is used to introduce small random changes on d independent columns to see d effect it will have on d model coefficients- Estimate column values in summary(mymodel), if dere will be big or small changes in d Estimate column values 
library(remotes)
remotes::install_github("JohnHendrickx/Perturb")

library(perturb)

p1 <- perturb(mymodel, pvars = c("income", "education"),   # income and education are d 2 independent numerical columns used in d linear model earlier. (1,1) is sd value for income and education columns
              prange = c(1,1))                            # dis performs 100 iterations as default
p1          
summary(mymodel)  # also 1st value in income column in $coeff.table is same value as income value under Estimate column in summary(mymodel) and also same with education value
summary(p1)


p2 <- perturb(mymodel, pvars = c("income", "education"),   # income and education are d 2 independent columns used in d linear model earlier. (1,1) is sd value for income and education columns
              prange = c(1,1), niter = 200)                            # niter = 200 means 200 iterations
p2        
summary(mymodel)
summary(p2)



# Perturbation Analysis with numerical and categorical independent variables
# lets create d linear model 1st using both numerical and categorical independent variables/columns
mymodel1 <- lm(prestige ~ income + education + type, Duncan)  # type column has categorical values
summary(mymodel1)

anova(mymodel1)

vif(mymodel1)  # d values under GVIF column are all < 10 so no collinearity problem


# perform d Perturbation analysis
p3 <- perturb(mymodel1, pvars = c("income","education"), 
              prange = c(1,1), pfac = list("type", pcnt = 95))   
p3    
summary(mymodel1)
summary(p3)       



#-------------------------------------------------------------------------------------------------
# dis is just for example/illustration
p3 <- perturb(mymodel1, pvars = c("income","education"), 
              prange = c(1,1), pfac = list("type", pcnt = 95), niter = 5)   # using 5 iterations
p3  

# NOTE: if you re-run d any of d perturb() functions- p1, p2, p3 above, u will get diff values compared to d previous value you get when u ran it before. 
# This is becus its is all randomized(there is no set.seed function involved so no repeat-ability of values). 
# To solve dis problem(ie so d values do not change anytime u re-run d perturb function): use set.seed function

set.seed(1234)

p4 <- perturb(mymodel1, pvars = c("income","education"), 
              prange = c(1,1), pfac = list("type", pcnt = 95), niter = 5)   # using 5 iterations
summary(p3) 

# as sure if u re-run dis p4 code above multiple times u will get d same values due to d set.seed()
#----------------------------------------------------------------------------------------------------------------------


