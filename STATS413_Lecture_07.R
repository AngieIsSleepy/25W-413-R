
medicorp <- read.csv("medicorp.csv")
str(medicorp)
attach(medicorp)

###################
# Interaction terms
###################

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

# How can we test to see if the slope on bonus varies across region?
lm.full <- lm(Sales~Bonus*Region + Advert*Region)
lm.reduced <- lm(Sales~Bonus + Advert*Region)
anova(lm.reduced, lm.full)
# Fail to reject the null that the slope for bonus doesn't vary by region

# Maybe we don't need to allow advert to vary across regions?
lm.full.new <- lm(Sales~Bonus + Advert*Region)
lm.reduced.new <- lm(Sales~Bonus + Advert + Region)
anova(lm.reduced.new, lm.full.new)
# Reject the null that the slope for advert doesnt vary by region
# Keep this interaction in the model

########################
# Continuous interaction
#######################

mall <- read.csv("mall_sales.csv")

sales <- mall$Sales....sq.ft.
income <- mall$Income..000.
competitors <- mall$Competitors

lm(sales~income + competitors)

# Add an interaction
lm(sales~income*competitors)

# Look at the hypothesis tests
summary(lm(sales~income*competitors))
# Looks like the interaction is not significant 
# (fail to reject t-test for beta_income:competitors = 0 with pval 0.3363

# We can get the same pval with anova and F-test with only one predictor in the null hypothesis
anova(lm(sales~income*competitors), lm(sales~income + competitors))

# If we remove the interaction, we have:
summary(lm(sales~income + competitors))
# Both income and competitors are now significant, so leave them in the model

medicorp <- read.csv("medicorp.csv")
str(medicorp)
attach(medicorp)
lm.interact <- lm(Sales~Region*Bonus + Region*Advert,x=T)
summary(lm.interact)
coef.interact <- lm.interact$coef

coef.Midwest <- c(coef.interact[1], coef.interact[4], coef.interact[5])
coef.South <- c(coef.interact[1] + coef.interact[2], coef.interact[4] + 
                  coef.interact[6], coef.interact[5] + coef.interact[8])
coef.West <- c(coef.interact[1] + coef.interact[3], coef.interact[4] + 
                 coef.interact[7], coef.interact[5] + coef.interact[9])

lm.full <- lm(Sales~Region*Bonus + Region*Advert)
lm.reduced <- lm(Sales~Region*Bonus + Advert)
anova(lm.reduced, lm.full)
Fstat <- anova(lm.reduced, lm.full)$F[2]
1-pf(Fstat, 2, 17)

####################
# Relationship between t and F
#####################
# Test null that Bonus = 0
lm.full2 <- lm(Sales~Bonus+Advert)
lm.reduced2 <- lm(Sales~Advert)
method1 <- anova(lm.reduced2, lm.full2)
Fstat <- method1$F[2]
pval1 <- method1$"Pr(>F)"[2]
pval1

method2 <- summary(lm.full2)
tstat <- method2$coef[2,3]
pval2 <- method2$coef[2,4]
pval2

tstat^2
Fstat

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
coef.South
