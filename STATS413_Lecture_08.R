
medicorp <- read.csv("medicorp.csv")
str(medicorp)
attach(medicorp)
lm.interact <- lm(Sales~Region*Bonus + Region*Advert,x=T)
summary(lm.interact)

############################
# Slopes with Interactions #
############################

X <- lm.interact$x
summary(lm.interact)
X <- lm.interact$x
RMSE <- summary(lm.interact)$sigma
RMSE
Vhat <- RMSE^2*solve(t(X)%*%X)

se <- sqrt(diag(Vhat))
se
# compare with...
summary(lm.interact)$coef[,2]

# Inference for slope on advert in the South:
names(lm.interact$coef)
a <- c(0, 0, 0, 0, 1, 0, 0, 1, 0)
betaAdSouth <- t(a) %*% coef(lm.interact)
betaAdSouth
# Var(\hat{\beta}_{Advert, South})
varhatAdSouth <- RMSE^2*t(a)%*%solve(t(X)%*%X)%*%a
seAdSouth <- sqrt(varhatAdSouth)
seAdSouth
# t-test
tstat <- (betaAdSouth - 0) / seAdSouth
pval <- 2 * (1 - pt(tstat, 17))
c(betaAdSouth, seAdSouth, tstat, pval)
# 95% CI
betaAdSouth[1, 1] + c(-1, 1) * qt(0.975, 17) * seAdSouth[1, 1]

# Note: you can actively tell R which category
# you want to be the reference category
# This provides another path for getting standard 
# errors
Region2 <- relevel(as.factor(Region), ref = "SOUTH")
# This makes South the reference category
lm2 <- lm(Sales~Region2*Bonus + Region2*Advert,x=T)
summary(lm2)

# compare SE on advert to:
seAdSouth

# compare Intercept, Slope on Bonus and Advert to:
coef.interact <- lm.interact$coef
coef.South <- c(coef.interact[1] + coef.interact[2], coef.interact[4] + 
                  coef.interact[6], coef.interact[5] + coef.interact[8])
coef.South

# Another example: Difference in slope between west and south
names(lm.interact$coef)
a <- t(t(rep(0, 9)))
a[8] <- 1
a[9] <- -1

# Var(\hat{\beta}_{Advert, South} - \hat{\beta}_{Advert, West})
varhatdiff <- RMSE^2*t(a)%*%solve(t(X)%*%X)%*%a
sediff <- sqrt(varhatdiff)
sediff

# Note: you can actively tell R which category
# you want to be the reference category
# This provides another path for getting standard 
# errors
Region2 <- relevel(as.factor(Region), ref = "SOUTH")
# This makes South the reference category
lm2 <- lm(Sales~Region2*Bonus + Region2*Advert,x=T)
summary(lm2)
# Look at standard error on Region2WEST:Advert
summary(lm2)$coef[9,2]

Region3 <- relevel(as.factor(Region), ref = "WEST")
# This makes West the reference category
lm3 <- lm(Sales~Region3*Bonus + Region3*Advert,x=T)
summary(lm3)
# Look at standard error on Region3South:Advert
summary(lm3)$coef[9,2]

###########################################
# Inference for conditional expectations  #
###########################################

fatherson <- read.csv("fatherson.csv")
attach(fatherson)
lm.fatherson <- lm(Son.Height~Father.Height, data = fatherson)
newfather <- data.frame(Father.Height = 76)
predict(lm.fatherson, newdata = newfather, interval = "confidence", level = 0.95)
# If you want to extract the standard error as well:
predict(lm.fatherson, newdata = newfather, interval = "confidence", level = 0.95, se=T)$se

# compare to what we derived
X <- model.matrix(lm.fatherson)
xtilde <- t(t(c(1,76)))
xtilde
RMSE <- summary(lm.fatherson)$sigma
se <- RMSE*sqrt(t(xtilde)%*%solve(t(X)%*%X)%*%xtilde)
se 

# Alternative form
H0 <- outer(rep(1, nrow(X)), rep(1, nrow(X)))/nrow(X)
Id <- diag(nrow(X))
RMSE*sqrt(1/nrow(X) + (xtilde[2] - mean(Father.Height))%*%
            solve(t(X[,2])%*%(Id - H0)%*%X[,2])%*%(xtilde[2] - mean(Father.Height)))

# Show where the confidence interval came from 
predict(lm.fatherson, newdata = newfather) + c(-1,1)*qt(0.975, nrow(fatherson)-2)*as.vector(se)
predict(lm.fatherson, newdata = newfather, interval = "confidence", level = 0.95)[,c(2,3)]

# Helps plot conf ints (catered to this data set)
ub.conf <- function(x, level = 0.95){
  predict(lm.fatherson, newdata = data.frame(Father.Height = x), 
          interval = "confidence", level = level)[,3]
}
lb.conf <- function(x, level = 0.95){
  predict(lm.fatherson, newdata = data.frame(Father.Height = x), 
          interval = "confidence", level = level)[,2]
}

# Plot Pointwise Confidence Intervals
plot(fatherson$Father.Height, fatherson$Son.Height, pch = 16, xlab = "Father", ylab = "Son", cex = .5)
points(mean(Father.Height), mean(Son.Height), col = "purple", pch = 8)
abline(lm.fatherson, col = "blue", lwd = 2)
curve(ub.conf(x, level = 0.95), from = 55, to = 85, add = T, col = "red", lty = 2)
curve(lb.conf(x, level = 0.95), from = 55, to = 85, add = T, col = "red", lty = 2)
legend("topleft", c("Prediction Equation", "Pointwise 95% Conf Interval", "Avg of Father, Son Heights"), 
       lty = c(1,2,NA), col = c("blue", "red", "purple"), pch = c(NA, NA, 8), bty = "n", cex = .8)

# Hypothesis test
predict(lm.fatherson, newdata = data.frame(Father.Height = 74), se=T)

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

