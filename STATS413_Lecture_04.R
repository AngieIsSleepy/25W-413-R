
medicorp <- read.csv("medicorp.csv")
str(medicorp)
attach(medicorp)
# sample size
n <- nrow(medicorp)

# One sample t test for the mean
mean(Sales)
sd(Sales)
sd(Sales)/sqrt(n)
tstat <- (mean(Sales)-1200)/(sd(Sales)/sqrt(n))
1-pt(tstat, df = n-1)

t.test(Sales, mu = 1200, alternative = "greater" )

# Hypothesis testing for regression coefficients ----

# multiple regression
lm.medicorp <- lm(Sales~Advert+Bonus, data = medicorp)
summary(lm.medicorp)

resids <- lm.medicorp$residuals
pred <- lm.medicorp$fitted.values

# Extract R2
# The definition of R2 value we use corresponds to 
# Multiple R2 in the summary, not adjusted R2
summary(lm.medicorp)
summary(lm.medicorp)$r.squared

# RMSE
RMSE <- summary(lm.medicorp)$sigma

# extract coefficients
coeff <- summary(lm.medicorp)$coefficients[,1]

# extract standard errors
standard.errors <- summary(lm.medicorp)$coefficients[,2]

# extract t statistics for testing null \beta = 0
tstat <- summary(lm.medicorp)$coefficients[,3]
tstat

# how are they computed?
coeff/standard.errors

# extract corresponding p-values with two-sided alternative
summary(lm.medicorp)$coefficients[,4]

# how are they computed?
2*(1-pt(abs(tstat), n-2-1))

