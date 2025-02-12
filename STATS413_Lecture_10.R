
# Mall sales
mall <- read.csv("mall_sales.csv")

sales <- mall$Sales....sq.ft.
income <- mall$Income..000.
competitors <- mall$Competitors
n <- length(competitors)
lm.both <- lm(sales~competitors+income, x = TRUE)

# extract residuals and fitted values
resid.both <- lm.both$residuals
fitted.both <- lm.both$fitted.values


########
# Heteroskedasticity
#########

# Standardized residuals
sresid.both <- rstandard(lm.both)

# Alternatively, can calculate using the definition
# get diagonals of hat matrix
X <- lm.both$x
H <- X%*%solve(t(X)%*%X)%*%t(X)
diaghat <- diag(H)
# alternative way
diaghat1 <- hat(cbind(competitors, income), intercept = T)

RMSE <- summary(lm.both)$sigma
sresid.both1 <- resid.both/(sqrt(1-diaghat1)*RMSE)

(sresid.both1 - sresid.both)

par(mfrow = c(1,3), mar=c(3,3,2,1), mgp = c(1.8,.5, 0))
plot(income, sresid.both, pch= 16, main = "Stand Residual vs Income", ylab = "Stand Residuals", xlab = "Income")
abline(h = 0, col = "red", lty = 2)
plot(competitors, sresid.both, pch= 16, main = "Stand Residual vs Competitors", ylab = "Stand Residuals", xlab = "Competitors")
abline(h = 0, col = "red", lty = 2)
plot(fitted.both,sresid.both, main = "Stand Residuals vs Fitted Values", xlab = "Predicted", ylab = "Stand Residuals",  pch = 16)
abline(h = 0, lty = 2, col = "red")
dev.off()

par(mfrow = c(1,3), mar=c(3,3,1,1), mgp = c(1.8,.5, 0), oma = c(0,0,1,0))
plot(income, sqrt(abs(sresid.both)), pch= 16, ylab = "", xlab = "Income")
abline(h = 0, col = "red", lty = 2)
plot(competitors, sqrt(abs(sresid.both)), pch= 16, ylab = "", xlab = "Competitors")
abline(h = 0, col = "red", lty = 2)
plot(fitted.both,sqrt(abs(sresid.both)), ylab = "",main = "", xlab = "Predicted",  pch = 16)
abline(h = 0, lty = 2, col = "red")
title(main = "sqrt(|standardized residuals|)", outer = T)
dev.off()


# An Example: Heteroskedasticity
x <- runif(100, -20, 20)
y <- x+rnorm(100, 0, 20+x)
hetreg <- lm(y~x)
sresid <- rstandard(hetreg)
hetfit <- hetreg$fitted.values


par(mfrow = c(1,2), mar=c(3,3,2,1), mgp = c(1.8,.5, 0))
plot(hetfit, sresid, pch= 16, main = "Standardized Residual vs Fitted", ylab = "Standardized Residuals", xlab = "Fitted")
abline(h = 0, col = "red", lty = 2)
plot(hetfit, sqrt(abs(sresid)), pch= 16, main = "sqrt(|Standardized Residual|) vs Fitted", ylab = "sqrt(|Standardized Residuals|)", xlab = "Fitted")

#############
# Normality #
#############

# Normality
qqnorm(sresid.both, pch = 16)
qqline(sresid.both)

# R generates certain diagnostic plots
# with its plot command.
plot(lm.both)



######################
# Potential Outliers #
######################

set.seed(413)

# Create a few data sets to visualize potential outliers
# One x value will be very far from the others
x <- c(rnorm(99), 20)
# The y value will follow the same distribution as for other values of x
y <- x+ rnorm(100, sd = 0.2)
par(mfrow = c(2,2), mar = c(3,3,2,1), mgp = c(1.8,.5,0))
hist(y, main = "")
plot(x,y, pch = 16)
points(x[100], y[100], col = "red", pch = 16, ylim = c(-8,8))
# What if the y value was not from the same distribution?
y2 <- y
y2[100] <- 5
hist(y2, main = "")
plot(x, y2, pch = 16, ylim = c(-8,8))
points(x[100], y2[100], col = "red", pch = 16)
# What happens to the regression lines in these two cases?
plot(x,y, pch = 16)
points(x[100], y[100], col = "red", pch = 16, ylim = c(-8,8))
abline(lm(y~x), col = "blue")
plot(x, y2, pch = 16, ylim = c(-8,8))
points(x[100], y2[100], col = "red", pch = 16)
abline(lm(y2~x), col = "blue")

# Can we use residuals to decide if there is an outlier?
par(mfrow = c(2,2), mar = c(3,3,2,1), mgp = c(1.8,.5,0))
y3 <- y
y3[100] <- 0
x3 <- x
x3[100]  <- 50
lm3 <- lm(y3~x3)
plot(x3, y3, pch = 16)
points(x3[100], y3[100], col = "red", pch = 16)
abline(lm3, col = "blue")
res3 <- lm3$residuals
hist(res3, main = "Hist of Residuals")
abline(v=res3[100], col = "red")

x4<-x
x4[100] <- 0
y4 <- y3
y4[100] <- 10
lm4 <- lm(y4~x4)
plot(x4, y4, pch = 16)
points(x4[100], y4[100], col = "red", pch = 16)

abline(lm4, col = "blue")
res4 <- lm4$residuals
hist(res4, main = "Hist of Residuals")
abline(v = res4[100], col = "red")

# What is the leverage of the 100th data point in each case?
par(mfrow = c(2,2), mar = c(3,3,2,1), mgp = c(1.8,.5,0))
lev3 <- hatvalues(lm3)
plot(x3, y3, pch = 16)
points(x3[100], y3[100], col = "red", pch = 16)
abline(lm3, col = "blue")
plot(x3, lev3, ylab = "Leverage")
points(x3[100], lev3[100], col = "red", pch = 16)

lev4 <- hatvalues(lm4)
plot(x4, y4, pch = 16)
points(x4[100], y4[100], col = "red", pch = 16)
abline(lm4, col = "blue")

plot(x4, lev4, ylab = "Leverage")
points(x4[100], lev4[100], col = "red", pch = 16)
dev.off()

par(mar = c(3,3,2,1), mgp = c(1.8,.5,0))
lm5 <- lm(y3[1:99]~x3[1:99])
plot(x3, y3, pch = 16)
points(x3[100], y3[100], col = "red", pch = 16)
abline(lm3, col = "blue")
abline(lm5, col = "green")
legend("top", c("All Observations", "Excluding Red"), col = c("blue", "green"), lty = c(1,1), bty = "n", cex = .8)

