#EDS241: Demonstrate that OLS is consistent (unbiased) and demonstrate omitted variable bias

library(estimatr)
library(stargazer)
library(ggplot2)

################################################################################
# 1. Demonstrate OLS estimate is consistent under LSA 1-3
################################################################################

set.seed(2244)
bigN <- 10000

# Generate X1 and u
X1 <- runif(bigN, min = 0, max = 10) #runif is random uniform dist
u <- rnorm(bigN, mean = 0, sd = 4). #rnorm is random normal dist 

# Bivariate population regression function
Y <- 5 + 1.5*X1 + u
population_data <- data.frame(X1, Y)

# OLS estimation, full sample
model1 <- lm(formula = Y ~ X1, data = population_data)
se_model1 <- starprep(model1, stat = c("std.error"), se_type = "HC1", alpha = 0.05)  #instead of taking normal standard error, take HC1.
stargazer(model1, se = se_model1, type="text")



# OLS estimation, with sample size increasing from n=2 to 10,000
betahat_output <- matrix(ncol = 2, nrow = bigN)

for (n in 1:bigN) {
  sample <- population_data[1:n,] 
  betahat_output[n,] <- lm(Y ~ X1, data = sample)$coefficients
} #take sample of size 1, 2, 3, ..., bigN, run regression, and store coefficients 

n <- seq(1,bigN)
forgraph <- data.frame(n , "beta0hat" = betahat_output[,1], "beta1hat" = betahat_output[,2]) #making dataframe with n size, intercept value, and beta one hat value. 

# Graphing the results
ggplot(forgraph , aes(x=n, y=beta1hat)) + geom_line(size=0.5, color="blue") +
  geom_hline(yintercept=1.5, size=2, color="red") +
  labs(x="n", y = "Beta1hat") + theme_bw() #plot each of the beta one hats from the previous for loop of regressions 

#THIS IS A CONSISTENT ESTIMATOR 


###############################################################################
# 2. Demonstrating omitted variables bias
###############################################################################

#x2 = x1 + a random normal term. X2 is X1 but with random noise
X2 = X1 +rnorm(bigN , mean=0 , sd=2.2)

# Multiple population regression function
Y <- 5 + 1.5*X1 + 10*X2 + u
population_data <- data.frame(X1, Y)

# OLS estimation, full sample, but ignoring X2
model2 <- lm(formula = Y ~ X1, data = population_data)
se_model2 <- starprep(model2, stat = c("std.error"), se_type = "HC1", alpha = 0.05) 
stargazer(model1, model2, se = c(se_model1, se_model2), type="text")


#

# Compute correlation between X1 and X2, and standard deviations
# Compute "probability limit" of Beta1_hat
cor(X1,X2)
sd(X1)
sd(X2)
1.5 + 10*cor(X1,X2)*sd(X2)/sd(X1)

# OLS estimation with omitted variable bias (OVB), with sample size increasing from n=1 to 10,000
betahat_output <- matrix(ncol = 2, nrow = bigN)

for (n in 1:bigN) {
  sample <- population_data[1:n,]
  betahat_output[n,] <- lm(Y ~ X1, data = sample)$coefficients
} 

n <- seq(1,bigN)
forgraph <- data.frame(n , "beta0hat" = betahat_output[,1], "beta1hat" = betahat_output[,2])

ggplot(forgraph , aes(x=n, y=beta1hat)) + geom_line(size=0.5, color="blue") +
  geom_hline(yintercept=1.5, size=2, color="red") +
  labs(x="n", y = "Beta1hat") + theme_bw()

#when you omit X2 from the model, since X2 is correlated with both the outcome and X1, the coefficient for X1 encapsulates the coef for X2 as well. 

#even though it tapers off and remains stbale, this does NOT mean that it is consistent. Would need to approach the true value. 



#### MULTICOLLINEARITY


#EDS241: Demonstrate problems with the OLS estimator when the regressors are multicollinear

library(estimatr)
library(stargazer)
library(ggplot2)
library(MASS)

#set.seed(145777)
#bigN <- 10000

set.seed(124578)
bigN <- 10000

# Generate X1 and X2, correlated regressors (here rho_12=0.5)
sigma<-rbind(c(1,0.5), c(0.5,1))
mu<-c(5,10) 
# generate the multivariate normal distribution
X <- data.frame(mvrnorm(n=bigN, mu=mu, Sigma=sigma))
X1 <- X[,1]
X2 <- X[,2]
cor(X1,X2)

# Generate u
u <- rnorm(bigN, sd = 2)

# Population regression function
Y <- 5 + 1.5*X1 + 7*X2 + u

RegData=data.frame(X1, X2, Y)
model1 <- lm(formula = Y ~ X1 + X2, data = RegData)
se_model1 <- starprep(model1, stat = c("std.error"), se_type = "HC1", alpha = 0.05) 




# Generate X1 and X2, correlated regressors (here rho_12=0.9)
sigma<-rbind(c(1,0.9), c(0.9,1))
mu<-c(5,10) 
# generate the multivariate normal distribution
X <- data.frame(mvrnorm(n=bigN, mu=mu, Sigma=sigma))
X1 <- X[,1]
X2 <- X[,2]
cor(X1,X2)

# Population regression function
Y <- 5 + 1.5*X1 + 7*X2 + u

RegData=data.frame(X1, X2, Y)
model2 <- lm(formula = Y ~ X1 + X2, data = RegData)
se_model2 <- starprep(model2, stat = c("std.error"), se_type = "HC1", alpha = 0.05) 




# Generate X1 and X2, correlated regressors (here rho_12=0.99999)
sigma<-rbind(c(1,0.99999), c(0.99999,1))
mu<-c(5,10) 
# generate the multivariate normal distribution
X <- data.frame(mvrnorm(n=bigN, mu=mu, Sigma=sigma))
X1 <- X[,1]
X2 <- X[,2]
cor(X1,X2)

# Population regression function
Y <- 5 + 1.5*X1 + 7*X2 + u

RegData=data.frame(X1, X2, Y)
model3 <- lm(formula = Y ~ X1 + X2, data = RegData)
se_model3 <- starprep(model3, stat = c("std.error"), se_type = "HC1", alpha = 0.05) 



# make table with estimated coefficients from all 3 regressions
stargazer(model1, model2, model3, se = c(se_model1, se_model2, se_model3), type="text")



