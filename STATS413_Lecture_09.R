
########################
# Prediction intervals #
########################

# Mall sales
mall <- read.csv("mall_sales.csv")

sales <- mall$Sales....sq.ft.
income <- mall$Income..000.
competitors <- mall$Competitors
n <- length(competitors)
lm.both <- lm(sales~competitors+income)
summary(lm.both)

xnew <- data.frame(income = 75, competitors = 3)
predict(lm.both, newdata = xnew)

sum(lm.both$coef*c(1, 3, 75))

# prediction interval for sales/sqft for stores 
# with three competitors and median income of 75K

predict(lm.both, newdata = xnew, interval = "prediction", level = .95)

predict(lm.both, newdata = xnew, interval = "confidence", level = .95)

# Another example of a prediction interval

fatherson <- read.csv("Fatherson.csv")
attach(fatherson)
lm.fatherson <- lm(Son.Height~Father.Height, data = fatherson)
newfather <- data.frame(Father.Height = 76)
predict(lm.fatherson, newdata = newfather, interval = "prediction", level = 0.95)

# compare to what we derived in class
X <- model.matrix(lm.fatherson)
xtilde <- t(t(c(1,76)))
xtilde
RMSE <- summary(lm.fatherson)$sigma
se.predict <- RMSE*sqrt(1+t(xtilde)%*%solve(t(X)%*%X)%*%xtilde)

# Prediction interval using formula:
predict(lm.fatherson, newdata = newfather) + 
  c(-1,1)*qt(0.975, nrow(fatherson)-2)*as.vector(se.predict)
# Compare to R built-in function:
predict(lm.fatherson, newdata = newfather, interval = "prediction", level = 0.95)[,c(2,3)]

###################################################
# Compare the prediction and confidence intervals #
###################################################

# Prediction interval
predict(lm.fatherson, newdata = newfather, interval = "prediction", level = 0.95)[,c(2,3)]
# Confidence interval
predict(lm.fatherson, newdata = newfather, interval = "confidence", level = 0.95)[,c(2,3)]

# Helps plot conf ints (catered to this data set)
ub.conf <- function(x, level = 0.95){
  predict(lm.fatherson, newdata = data.frame(Father.Height = x), interval = "confidence", level = level)[,3]
}
lb.conf <- function(x, level = 0.95){
  predict(lm.fatherson, newdata = data.frame(Father.Height = x), interval = "confidence", level = level)[,2]
}

# Helps plot pred ints (catered to this data set)
ub.pred <- function(x, level = 0.95){
  predict(lm.fatherson, newdata = data.frame(Father.Height = x), interval = "prediction", level = level)[,3]
}
lb.pred <- function(x, level = 0.95){
  predict(lm.fatherson, newdata = data.frame(Father.Height = x), interval = "prediction", level = level)[,2]
}

# Plot Pointwise Confidence and Prediction Intervals
plot(fatherson$Father.Height, fatherson$Son.Height, pch = 16, xlab = "Father", ylab = "Son", cex = .5, ylim = c(60, 80))
abline(lm.fatherson, col = "blue", lwd = 2)
curve(ub.conf(x, level = 0.95), from = 45, to = 95, add = T, col = "red", lty = 2, lwd = 2)
curve(lb.conf(x, level = 0.95), from = 45, to = 95, add = T, col = "red", lty = 2, lwd = 2)
curve(ub.pred(x, level = 0.95), from = 45, to = 95, add = T, col = "purple", lty = 2, lwd = 2)
curve(lb.pred(x, level = 0.95), from = 45, to = 95, add = T, col = "purple", lty = 2, lwd = 2)
legend("topleft", c("Prediction Equation", "Pointwise 95% Conf Interval", "Pointwise 95% Pred Interval"), lty = c(1,2,2), col = c("blue", "red", "purple"), bty = "n", cex = .8)

# Compare prediction and confidence intervals for the new store in the mall

# prediction interval for sales/sqft for stores 
# with three competitors and median income of 75K

predict(lm.both, newdata = xnew, interval = "prediction", level = .95)
predict(lm.both, newdata = xnew, interval = "confidence", level = .95)
# Confidence interval is MUCH narrower!

# read in our data

mall <- read.csv("mall_sales.csv")
sales <- mall$Sales....sq.ft.
income <- mall$Income..000.
competitors <- mall$Competitors
n <- length(competitors)
lm.both <- lm(sales~competitors+income,x=T)
summary(lm.both)


##################################
# Assumption Checking
###############################

# extract residuals and fitted values
resid.both <- lm.both$residuals
fitted.both <- lm.both$fitted.values


# Linearity:
par(mfrow = c(1,3), mar=c(3,3,2,1), mgp = c(1.8,.5, 0))
plot(income, resid.both, pch= 16, main = "Residual vs Income", ylab = "Residuals", xlab = "Income")
abline(h = 0, col = "red", lty = 2)
plot(competitors, resid.both, pch= 16, main = "Residual vs Competitors", ylab = "Residuals", xlab = "Competitors")
abline(h = 0, col = "red", lty = 2)
plot(fitted.both,resid.both, main = "Residuals vs Fitted Values", xlab = "Predicted", ylab = "Residuals",  pch = 16)
abline(h = 0, lty = 2, col = "red")
dev.off()

