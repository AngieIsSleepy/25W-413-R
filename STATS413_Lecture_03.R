library(MASS)
# read in the data
mall <- read.csv("mall_sales.csv")

# calculate sample size
n <- nrow(mall)
# rename column names
colnames(mall) <- c("sales", "income", "competitors", "visitors")

attach(mall)
# "." shortcut notation: using the data set mall
# compute a regression with sales as the y variable
# and all other variables and predictors
lm.mall <- lm(sales~., data= mall)
summary(lm.mall)

# ask R for the "X" matrix it used to perform the regression
X <- model.matrix(lm.mall)
X


# Orthogonality of residuals and predictors ----

# show this with ``competitors''
competitors <- mall$competitors
sum(competitors*lm.mall$residuals)


# Hat matrix ----

# Define the hat matrix
# H = X (X^T X)^{-1}X^T

H <- X%*%solve(t(X)%*%X)%*%t(X)

# Also define the nxn identity matrix

Id <- diag(1,n,n)
Id

# fitted values using H formula, compare with R output
yhat_H <- H%*%sales
cbind(yhat_H, lm.mall$fitted.values)

# residuals using H formula, compare with R output
resid_H <- (Id - H)%*%sales
cbind(resid_H, lm.mall$residuals)


#### Visualizing the variability ----

# Visualizing the data generating process, along with 
# the variability in the slope with simple regression
library(MASS)
n <- 50
# fix the locations for our 50 x variables
# we'll draw them from a uniform distribution,
# but any distribution would be ok
x <- runif(n, 0, 1)

# Let's decide that the true slope equals 5,
# the true intercept equals 1, and the true standard 
# deviation for the error terms equals 2
beta0 <- 1
beta1 <- 5
sigma.epsilon <- 2

# now, we'll simulate multiple data sets at these design points
nsim <- 10
cols <- sample(rainbow(nsim))

# create a matrix to store sample intercept (first column)
# and slope (second column)
bhatmatrix <- matrix(0, nsim, 2)

for(i in 1:nsim)
{
  plot(x, runif(n), main = "", ylab = "", xlab = "", ylim = c(-1, 8), type="n")
  # for the ith data set, simulate our n noise terms
  epsilon <- mvrnorm(1, mu = rep(0, n), Sigma = diag(sigma.epsilon^2, n))
  # now, each response equals its expectation (beta0 + beta1*x_i)
  # plus a noise term
  Y <- beta0 + beta1*x + epsilon
  
  # compute the regression for the ith data set
  lmi <- lm(Y~x)
  # store the coefficients
  bhatmatrix[i,] <- lmi$coef
  # plot the regression lines from data sets past and present
  for(j in 1:i)
  {
    abline(a= bhatmatrix[j,1], b = bhatmatrix[j,2], col = cols[j], lwd = .5)
  }
  abline(a= bhatmatrix[i,1], b = bhatmatrix[i,2], col = cols[i], lwd = 3)
  
  # plot the points in the current data set
  points(x, Y, col = cols[i], pch = 16)
  Sys.sleep(.5)
}

# repeat it now without the animation to assess
# distribution of sample slope and intercept
nsim <- 10000
bhatmatrix <- matrix(0, nsim, 2)
for(i in 1:nsim)
{
  epsilon <- mvrnorm(1, mu = rep(0, n), Sigma = diag(sigma.epsilon^2, n))
  Y <- beta0 + beta1*x + epsilon
  lmi <- lm(Y~x)
  bhatmatrix[i,] <- lmi$coef
}

# What do our 10,000 lines look like now?
cols <- sample(rainbow(nsim))
plot(x, runif(n), main = "", ylab = "", xlab = "", ylim = c(-1, 8), type="n")
for(j in 1:nsim) {
  abline(a= bhatmatrix[j,1], b = bhatmatrix[j,2], col = cols[j], lwd = .5)
}

# expectation of beta
E.bhat <- c(beta0, beta1)
E.bhat
# compare with average across simulations
colMeans(bhatmatrix)

# get the design matrix
Xmat <- model.matrix(lmi)

# compute the true variance-covariance matrix
V.bhat <- sigma.epsilon^2*solve(t(Xmat)%*%Xmat)
V.bhat

# Extract the diagonals of V.bhat
# These are the variances for betahat_j
diag(V.bhat)

# Extract the covariance between (hatbeta_0, hatbeta_1)
V.bhat[1,2]
V.bhat[2,1]

# compare with covariance across simulations
V.bhat
cov(bhatmatrix)

# variance of sample intercept
var(bhatmatrix[,1])

# variance of sample slope
var(bhatmatrix[,2])

# covariance between the two
cov(bhatmatrix[,1], bhatmatrix[,2])



#### R Squared ----

summary(lm.mall)
Rsquare <- summary(lm.mall)$r.squared

resid.mall <- lm.mall$residuals
fitted.mall <- lm.mall$fitted.values

SSErrors <- sum(resid.mall^2)
SSTotal <- sum((sales - mean(sales))^2)

1 - SSErrors/SSTotal
Rsquare
cor(fitted.mall, sales)^2

# simple regression Rsquared
lm.income <- lm(sales~income)
summary(lm.income)$r.squared
cor(income, sales)^2
cor(lm.income$fitted.values, sales)^2

# Deficiency of Rsquare
# Show that Rsquare always increases as you add more covariates

# our original model:
summary(lm(sales~competitors+income+visitors))$r.squared
# simulate a garbage covariate unrelated to sales

xgarbage <- rnorm(length(sales))
# rsquared in our new model, including the garbage
summary(lm(sales~competitors+income+visitors+xgarbage))$r.squared

# simulate 20 more garbage covariates, add them to the model
XG <- matrix(rnorm(20*length(sales)), nrow = length(sales), ncol = 20)
summary(lm(sales~competitors+income+visitors+xgarbage+XG))$r.squared

