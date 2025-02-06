
medicorp <- read.csv("medicorp.csv")
str(medicorp)
attach(medicorp)


# multiple regression
lm.medicorp <- lm(Sales~Advert+Bonus)a
summary(lm.medicorp)


# Test Null \beta_advert = 3, alternative \beta_advert < 3
# t-test where we recalculate the t statistic will answer this

coeffs <- lm.medicorp$coefficients
coeffs
se <- summary(lm.medicorp)$coeff[,2]

tstat <- (coeffs[2] - 3)/se[2]
tstat

pt(tstat, 23)

# Since the p-value of 0.12 is greater than alpha = 0.05, 
#   there is not significant evidence that a store with the same bonus spend
#   but one hundred more spent on advertising has an expected 
#   increase of less than 3 thousand dollars in sales.

# 90% confidence interval for advertisement

coeffs[2] + c(-1,1)*qt(.95,23)*se[2]

# Is a model with both bonus and advert substantially better than a model with only advert?
# t-test for beta_bonus = 0 answers this question

summary(lm.medicorp)$coefficient
summary(lm.medicorp)$coefficients[3, 4]

# Reject the null that bonus does not improve model performance beyond advert
#  since the pvalue is less than alpha

# Is multiple regression with both bonus and advertisement better than a model with just an intercept?
# F test for beta_bonus=beta_advert=0 answers this question

summary(lm.medicorp)
summary(lm.medicorp)$fstatistic
summary(lm.medicorp)$fstatistic[1]

# Can calculate using the formula as well
rsq <- summary(lm.medicorp)$r.squared
(rsq / (1-rsq)) * (26-2-1)/2

# overall F p-value calculation
# pf is CDF of F distribution with (2,23) degrees of freedom
# p-value given in the summary, but no way to extract it without calculating
summary(lm.medicorp)
1 - pf(90.47, 2, 23)
1 - pf(summary(lm.medicorp)$fstatistic[1], summary(lm.medicorp)$fstatistic[2], 
       summary(lm.medicorp)$fstatistic[3])

# relationship of sales across region
sort(tapply(Sales, Region, mean))
boxplot(Sales~Region, cex.lab = 1.5, cex.axis = 1.5)
abline(h = mean(Sales), col = "red", lwd = 1.5)
# total sum of squares
sum((Sales - mean(Sales))^2)

# make region-specific data sets
Sales.Midwest <- Sales[Region=="MIDWEST"]
Sales.South <- Sales[Region=="SOUTH"]
Sales.West <- Sales[Region=="WEST"]


# region is categorical
Region
# What happens if I run a regression with a categorical?
summary(lm(Sales~Region))


# hmmm.....what did R do?

# what do the covariates R uses look like
xmat <- lm(Sales~Region, x = T)$x
head(xmat)

#################################
# What happens when I include all categories as 
# dummy variables?
South <- (Region=="SOUTH")*1
West <- (Region=="WEST")*1
Midwest <- (Region=="MIDWEST")*1

Xprop <- cbind(rep(1, length(South)), South, West, Midwest)
solve(t(Xprop)%*%Xprop) # Error! Singularity
lm(Sales~South+West+Midwest) # Treats midwest as N/A
###########################

# Back to the usual way to do regression with categoricals
# here are the coefficients
lm(Sales~Region)$coefficients

# componentwise, they equal...
mean(Sales.Midwest)
mean(Sales.South) - mean(Sales.Midwest)
mean(Sales.West) - mean(Sales.Midwest)

boxplot(Sales~Region, cex.lab = 1.5, cex.axis = 1.5)
abline(h = mean(Sales[Region == "SOUTH"]), col = "red", lwd = 1.5)
abline(h = mean(Sales[Region == "MIDWEST"]), col = "red", lwd = 1.5)
abline(h = mean(Sales[Region == "WEST"]), col = "red", lwd = 1.5)
# Residual sum of squares
sum((Sales.Midwest - mean(Sales.Midwest))^2) +
  sum((Sales.West - mean(Sales.West))^2) +
  sum((Sales.South - mean(Sales.South))^2)



# relationship between bonus and region, advert and region
boxplot(Bonus~Region)
boxplot(Advert~Region)
# maybe intrinsic regional differences aren't to cause for the 
# large average differences...

# Can we adjust for bonus, advert, and region all at once?
lm(Sales~Region+Bonus+Advert)
# how does the intercept here compare to that when we only included region?
lm(Sales~Region)$coefficients




# add interaction terms - allow for different slopes
summary(lm(Sales~Region*Bonus + Region*Advert))
# what do the covariates R uses look like
xmat2 <- lm(Sales~Region*Bonus + Region*Advert, x = T)$x
head(xmat2)

coef.interact <- lm(Sales~Region*Bonus+Region*Advert)$coef


# compare this to separate multiple regressions
lm(Sales~Bonus + Advert, subset = (Region == "MIDWEST"))
c(coef.interact[1], coef.interact[4], coef.interact[5])

lm(Sales~Bonus + Advert, subset = (Region == "SOUTH"))
c(coef.interact[1] + coef.interact[2], coef.interact[4] + coef.interact[6], 
  coef.interact[5] + coef.interact[8])

lm(Sales~Bonus + Advert, subset = (Region == "WEST"))
c(coef.interact[1] + coef.interact[3], coef.interact[4] + coef.interact[7], 
  coef.interact[5] + coef.interact[9])

# Can we think of a benefit from pooling information across samples,
# even if in reality all the lines were different by region?


# Overall F test
summary(lm(Sales~Bonus*Region + Advert*Region))


