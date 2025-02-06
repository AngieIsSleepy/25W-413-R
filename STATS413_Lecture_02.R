
####################
#Multiple Regression - Mall Sales
#####################

# read in our data

mall <- read.csv("mall_sales.csv")
sales <- mall$Sales....sq.ft.
income <- mall$Income..000.
competitors <- mall$Competitors
n <-length(competitors)


#### Individual simple regressions ----

# simple regression - sales on income
par(mgp = c(1.8,.5,0), mar = c(3,3,2,1))
cor(income, sales)
lm.income <- lm(sales~income) 
plot(income, sales, pch = 16, main = "Regression of Sales on Income")
abline(lm.income, lwd=  2)
lm.income

# simple regression - sales on competitors
lm.competitors <- lm(sales~competitors)
cor(sales, competitors)
plot(competitors, sales, pch = 16, main = "Regression of Sales on Competitors")
abline(lm.competitors, lwd = 2)
lm.competitors

# scatterplot matrix; pairs of correlations
M <- cbind(sales, income, competitors)
pairs(M, pch = 16)
cor(M)

# Sales and income are positively correlated.
# So are income and competitors!
# From the 2d scatterplot, doesn't seem 
# like much is happening with sales and competitors
# What about lurking variables???

# If you want to look in 3 dimensions, you can uncomment the code below
# Mac users - you'll need to install Xquartz to display the 3D graphics below

# library(car)
# library(rgl)
# scatter3d(sales~income + competitors, surface = F)


#### Multiple regression ----

# run a regression to predict sales/sqft, this time using both predictor variables
# Median Income (in 1000s)
# Competitor Stores
lm.both <- lm(sales~competitors+income)
lm.both

# Another pretty plot that you can skip
# scatter3d(sales~income + competitors, surface = T)

# fitted values, residuals
fitted.both <- lm.both$fitted.values
resid.both <- lm.both$residuals

# in R, we can form predictions from a model we've already fit using the 
# predict command
# it takes as input (1) a model; and (2) a data set with column names equal to those
# of the data set used to fit the model
newmall <- data.frame(competitors=2, income = 57) # new data
predict(lm.both, newmall)

# more advanced summary output! we'll need to develop
# a bit of theory to understand this.
summary(lm.both)


#### Multiple regression by hand ----

# Let's see how the coefficients are computed:
# ask R for the "X" matrix it used to perform the regression
X <- model.matrix(lm.both)
X
colnames(X)

# Let's use our derived formula to compute the intercept and slopes
# Recall our formula betahat = (X^T X)^{-1}X^T y

# Matrix operations in R
# solve() performs matrix inversion
# t() transposes matrix
# %*% performs matrix multiplication (as opposed to elementwise) 

hatbeta <- solve(t(X)%*%X)%*%t(X)%*%sales
hatbeta
# compare with R output
lm.both$coefficients

# Let's do the same for the fitted values and residuals
yhat <- X%*%hatbeta
resid <- sales - X%*%hatbeta

# compare to the fitted values and residual we previously extracted 
cbind(yhat, fitted.both)
cbind(resid, resid.both)

# predictions at new values of x by hand: yhat = x^T betahat
xnew <- c(1, 2, 57)
t(xnew)%*%hatbeta
# Same as we got before using the predict() function

