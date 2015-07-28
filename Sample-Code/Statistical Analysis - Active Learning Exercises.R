#######
# GLM #
#######

# Load the data
library(ggplot2)
data(mpg)

# Assess the outcome variable - cty
summary(mpg$cty)
hist(mpg$cty)
qqnorm(mpg$cty)
qqnorm(log(mpg$cty))
# Data looks a bit more 'normal' after logging
qqplot(mpg$cty, rpois(1000, lambda=25))
# I took 1000 random draws from a Poisson distribution
# with lambda (rate) of 25 and compared its distribution
# to the distribution of cty. Looks pretty good.
# Can model as normal using log(cty) or Poisson using cty.

# The outcome seems to be approximately normal.
# We can assume that the observations are independent.
# (No reason to believe otherwise.)

boxplot(log(mpg$cty) ~ mpg$trans)
# Spread of data (the box of the boxplots) seem roughly the same.
# We can assume equal variances.

mean(mpg$cty)
var(mpg$cty)
# If we are interested in Poisson regression, we must make
# sure that the mean equals the variance. The variance is
# slightly larger, but not too much.

# Experiment with different predictors
fit1 <- lm(log(cty) ~ trans, data=mpg)
fit2 <- lm(log(cty) ~ trans + class, data=mpg)
fit3 <- lm(log(cty) ~ trans + class + drv, data=mpg)
fit4 <- lm(log(cty) ~ trans + class + drv + as.factor(cyl), data=mpg)
fit5 <- lm(log(cty) ~ trans + class + drv + as.factor(cyl) + manufacturer, data=mpg)
# I like to rely on scientific knowledge in choosing predictors.
# An argument can be made for the 4th or 5th model.
# Either way, no transmission type is significantly associated with
# city miles per gallon.

# Fit the same models as Poisson regression
fit6 <- glm(cty ~ trans, data=mpg, family="poisson")
fit7 <- glm(cty ~ trans + class, data=mpg, family="poisson")
fit8 <- glm(cty ~ trans + class + drv, data=mpg, family="poisson")
fit9 <- glm(cty ~ trans + class + drv + as.factor(cyl), data=mpg, family="poisson")
fit10 <- glm(cty ~ trans + class + drv + as.factor(cyl) + manufacturer, data=mpg, family="poisson")
# Transmission still is not significantly associated with city mpg.
# However, lots of predictors that were significant in the linear
# model are not significant in this one.

# How to decide which to use? Use your judgment and explain your
# reasoning.

#######
# GEE #
#######

# Load the data
library(geepack)
data(respiratory)

# We have observations of people at multiple time
# points, hence longitudinal data. We should account
# for possible correlation within a cluster (person).

# NOTE: For outcome, 1 is "good", 0 is "bad".

# Perform EDA
table(respiratory$treat, respiratory$outcome)
chisq.test(respiratory$treat, respiratory$outcome)
# Significant difference in outcome proportion between treatments.
boxplot(respiratory$age ~ respiratory$outcome)
# Distribution for age does not differ much by outcome.
# However, scientific reasoning would tell us to include
# age in model.
table(respiratory$baseline, respiratory$outcome)
# Respiratory status is likely to not change so we should
# adjust for baseline status.
table(respiratory$sex, respiratory$outcome)

# Fit GEE model with different correlation structures
fit.ind <- geeglm(outcome ~ treat + baseline + age + sex,
                  data=respiratory, id=id, family="binomial",
                  corstr="independence")
fit.exch <- geeglm(outcome ~ treat + baseline + age + sex,
                  data=respiratory, id=id, family="binomial",
                  corstr="exchangeable")
fit.ar <- geeglm(outcome ~ treat + baseline + age + sex,
                   data=respiratory, id=id, family="binomial",
                   corstr="ar1")
# The model estimates and SEs does not differ much between
# independent and exchangeable model. However, the estimates
# and SEs change some for AR1 model. Nevertheless, the findings
# are the same. Treatment P is associated with lower odds of
# "good" respiratory status.

#######################
# Wald Test and Power #
#######################

### Wald Test ###

# Load the data
data(cars)

# Fit linear regression of speed on distance
linear.fit <- lm(speed ~ dist, data=cars)

# Load package needed for Wald Test
library(aod)

# Perform the Wald Test
wald.test(b=coef(linear.fit), Sigma=vcov(linear.fit), Terms=2)

# Compare with model's F statistic and coef's p-value
summary(linear.fit)

### Power ###

# Load package for power calculations
library(pwr)

# Sample size calculation for t-test
pwr.t.test(sig.level=0.05, type="two.sample", power=0.8, d=0.5)

# Decrease power
pwr.t.test(sig.level=0.05, type="two.sample", power=0.5, d=0.5)

# Increase power
pwr.t.test(sig.level=0.05, type="two.sample", power=0.95, d=0.5)

# Decrease effect size
pwr.t.test(sig.level=0.05, type="two.sample", power=0.8, d=0.25)

# Increase effect size
pwr.t.test(sig.level=0.05, type="two.sample", power=0.8, d=1)

# As power decreases or effect size increases, the sample size
# needed decreases. As power increases or effect size decreases,
# the sample size needed increases.

##############
# Prediction #
##############

# Load package for CART models
library(rpart)

# Load data set
data(car.test.frame)

# Fit CART prediction model
tree.fit <- rpart(Price~Type+Mileage+Reliability+Country,
                  data=car.test.frame)
tree.fit

# Plot model tree
plot(tree.fit)
text(tree.fit)
# These commands do not produce the clearest plots

# Load a package for producing rpart plots
library(rpart.plot)

# Plot the tree
rpart.plot(tree.fit)
# Much better!

# To describe, if a car is a small type, the predicted price
# is 7680. If a car is any other type, we move to the second branch.
# In that branch, if a car was produced in Korea, USA, or Japan/USA,
# we move to the branch on the left. If not, the estimated price
# is 16100. In the branch on the left, if the car is compact or 
# sporty, the predicted price is 11100. If not, the predicted price
# is 14200.

