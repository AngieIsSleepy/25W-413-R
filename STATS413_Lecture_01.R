# Simple Regression - Galton's Height Data 

#### Explore the data, z-scores, and correlation ----

fatherson <- read.csv("fatherson.csv")
father <- fatherson$Father.Height
son <- fatherson$Son.Height

plot(father, son, main = "Father's and Son's Heights", 
     xlab = "Father's Height (in)", ylab = "Son's Height (in)", pch=16)

# Form Z-scores for father and son
zfather <- (father-mean(father))/sd(father)
mean(zfather)
sd(zfather)

zson <- (son - mean(son))/sd(son)
mean(zson)
sd(zson)

# Number of observations
n <- length(son)

# Show correlation
cor(father, son)
(1/(n-1))*sum(zfather*zson)

cor(zfather, zson)

# Compare scatterplots before and after z-scoring. What changes about the plots?
plot(son, father, pch = 16)
plot(zson, zfather, pch = 16)


#### Generate the fancy color plots ----
# (You can skip this part)

vs <- fatherson$Vert.Strip
unique.vs <- sort(unique(vs))
mean.vs <- tapply(son, vs, mean)
colvec <- rainbow(7)[match(vs, unique.vs)]

plot(father, son, main = "Father's and Son's Heights", xlab = "Father's Height (in)", 
     ylab = "Son's Height (in)", col = colvec, pch = 16)
points(unique.vs[c(2,4,6)], mean.vs[c(2,4,6)], pch = 18, cex = 3)
abline(lm(son~father), lwd = 3)
abline(v = c(66, 68, 70 ,72, 74, 76), lwd = 2, lty = 2, col = "black")


#### Running simple regression in R ----

# General syntax: lm(y~x)
sonreg <- lm(son~father)
sonreg

# Show scatterplot with regression line included
plot(father, son, pch = 16)
abline(sonreg)

# Slope
bhat1 <- cor(son, father)*sd(son)/sd(father)
bhat1

# Slope and intercept coefficients as reported by regression object

sonreg$coef

# Extract slope coefficient from regression object
sonreg$coef[2]

# Intercept
bhat0 <- mean(son) - bhat1*mean(father)
bhat0

# Extract intercept coefficient from regression object
sonreg$coef[1]

# Fitted values
sonreg$fitted.values

# Residuals
sonreg$residuals


#### How does this compare to running regression on the z-scores? ----

zreg <- lm(zson~zfather)
zreg

# Intercept is a numeric zero; slope equals correlation!
cor(son, father)

# Show scatterplot of z-scores, with z-score regression line
plot(zson, zfather, pch = 16)
abline(zreg)




