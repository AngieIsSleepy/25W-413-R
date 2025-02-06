
medicorp <- read.csv("medicorp.csv")
str(medicorp)
attach(medicorp)
# sample size
n <- nrow(medicorp)
# multiple regression
lm.medicorp <- lm(Sales~Advert+Bonus, data = medicorp)
summary(lm.medicorp)
coeff <- summary(lm.medicorp)$coefficients[,1]
standard.errors <- summary(lm.medicorp)$coefficients[,2]
# extract t statistics for testing null \beta = 0
tstat <- summary(lm.medicorp)$coefficients[,3]
tstat

# how are they computed?
coeff/standard.errors

# extract corresponding p-values with two-sided alternative
summary(lm.medicorp)$coefficients[,4]

# how are they computed?
2*pt(-abs(tstat), n-2-1)
2*(1-pt(abs(tstat), n-2-1))
(1-pt(abs(tstat), n-2-1))+pt(-abs(tstat), n-2-1)

# 95% two-sided confidence interval
coeff[3] + c(-1,1)*qt(.975, n-2-1)*standard.errors[3]

# easy way to get two-sided confidence intervals
confint(lm.medicorp, level = .95)

# 95% one-sided confidence interval if interested in the beta_j > gamma_0 case
coeff[3] - qt(.95, n-2-1)*standard.errors[3]

# Would have to reverse-engineer the confint function to get a one-sided interval
# Double the alpha value, and then only look at the relevant side of the interval
confint(lm.medicorp, level = 0.90)

###################
# Null distribution for \hat{\beta}_j
####################


beta0 <- -680
beta1 <- 2.7
beta2 <- 2.1
beta <- c(beta0, beta1, beta2)
# tstat's distribution
nsim <- 20000
b.sim  <- matrix(0, nsim, 3)
se.sim <- matrix(0, nsim, 3)
confint.sim <- matrix(0, nsim, 2)
sigma.error <- 88
  for(i in 1:nsim)
  {
    # for the ith data set, simulate our n noise terms
    errors <- rnorm(n, 0, sigma.error)
    # now, each response equals its expectation (beta0 + beta1*x_1i + beta_2x_2i)
    # plus a noise term
    Sales.sim <- beta0 + beta1*Advert+beta2*Bonus+ errors
    
    # compute the regression for the ith data set
  
    lmi <- lm(Sales.sim~Advert+Bonus)
    # store the coefficients
    b.sim[i,] <- lmi$coef
    # store the standard errors
    se.sim[i,] <- summary(lmi)$coef[,2]
    confint.sim[i,] <- confint(lmi)[3,]
    
  }
X <- model.matrix(lmi)
SD.beta <- sigma.error*sqrt(diag(solve(t(X)%*%X)))

# What's (\hat{\beta} - \beta)/SD(\beta)'s distribution?
# Let's look at second coefficient
hist((b.sim[,3] - beta2)/SD.beta[3], main = "Z", freq = F)
curve(dnorm(x), from = -5, to = 5, add = T, col = "purple", lwd = 2)
legend("topleft", "Normal(0,1)", bty = "n", col = "purple", lwd = 2)

qqnorm(b.sim[,3])
qqline(b.sim[,3])

# Now, replace SD with se()
tstat <- (b.sim[,3] - beta2)/se.sim[,3]
hist(tstat, main = "T", freq = F)

qqnorm(tstat)
qqline(tstat)

tstat <- (b.sim[,3] - beta2)/se.sim[,3]
hist(tstat, main = "T", freq = F)
curve(dt(x,n-2-1), from = -5, to = 5, add = T, col = "purple", lwd = 2)
legend("topleft", "t with 23 df", bty = "n", col = "purple", lwd = 2)


###################
# What does 95\% confident mean?
# Let's see how often our confidence intervals 
# For \beta_2 captured the truth
####################

cover <- (beta2 >= confint.sim[,1] & beta2 <= confint.sim[,2])
# make a color correspondence for plotting purposes
colvec <- ifelse(cover==1, "forestgreen", "red")

dev.off()
# plot the ranges of 100 confidence intervals, along with true mean
plot(confint.sim[,1], confint.sim[,2], type = "n", ylim = c(-1, 101), 
     xlim = c(min(confint.sim), max(confint.sim)), ylab = "Confidence Intervals", 
     xlab = "", yaxt = "n", main = "One Hundred 95% Confidence Intervals")

legend("topright", c("Good Interval", "Bad Interval", "Population Slope (2.1))"), 
       lty = c(1,1,2), col=c("forestgreen", "red", "blue"), cex = .5)

abline(v = beta2, lty = 2, col = "blue")
for(i in 100:1)
{
  segments(confint.sim[i,1], i, confint.sim[i,2], col = colvec[i])
}

mean(cover)



###################
# Overall F test
####################
#library(car)

beta0 <- 1200
sigma.error <- 90

# simulate without animation to see
# Fstat's distribution
nsim <- 5000
Rsquared.sim <- rep(0, nsim)
fstat.sim <- rep(0, nsim)
b.sim  <- matrix(0, nsim, 3)

for(i in 1:nsim)
{
  # for the ith data set, simulate our n noise terms
  errors <- rnorm(n, 0, sigma.error)
  # now, each response equals its expectation (beta0)
  # plus a noise term
  Sales.sim <- beta0 + errors
  
  # compute the regression for the ith data set
  # note that in reality, in this simulation,
  lmi <- lm(Sales.sim~Advert+Bonus)
  b.sim[i,] <- lmi$coef
  # store the coefficients
  Rsquared.sim[i] <- summary(lmi)$r.squared
  fstat.sim[i] <- summary(lmi)$fstatistic[1]
}

# What is the Fstatistic's distribution under the null?
hist(fstat.sim, freq = F, breaks = 20, 
     main = expression(paste("Simulated Histogram of ", F["stat"], " with True Distribution Overlaid")), 
     xlab = expression(paste(F["stat"])))
curve(df(x, 2, n-2-1), from = 0, to = 10, add = T, col = "purple", lwd = 2)
legend("top", expression(F["2, 23"]), bty = "n", col = "purple", lwd = 2)

summary(lm.medicorp)
summary(lm.medicorp)$fstatistic

fvec <- summary(lm.medicorp)$fstatistic

# fvec is a vector of length three
# first the value, then the first degree of freedom, then second
fvec[1]

R2 <- summary(lm.medicorp)$r.squared
R2/(1-R2)*(n-2-1)/(2)

# oddly summary(lm.medicorp) doesn't return the pvalue directly for the
# test despite outputting it
pval <- 1-pf(fvec[1], fvec[2], fvec[3])
pval

summary(lm.medicorp)


# compare the equivalent forms of the overall F statistic:
RSS <- sum(lm.medicorp$residuals^2)
TSS <- (n-1)*var(Sales)

((TSS-RSS)/2)/(RSS/(n-2-1))
R2/(1-R2)*(n-2-1)/(2)
fvec[1]


