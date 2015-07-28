##############
# NUMBER ONE #
##############

# Load the data
require(car)
data(Freedman)

# Fit the required linear model
linear.fit <- lm(crime~density+nonwhite+population, data=Freedman)

# Save residuals and fitted values
res <- residuals(linear.fit)
stu.res <- rstudent(linear.fit)
fit.value <- fitted(linear.fit)

# Create residual plots
smoother <- smooth.spline(fit.value, res)
plot(fit.value, res)
abline(h=0, lty=2)
lines(smoother, col="red")
# The smoother deviates from zero. While some of the
# deviation is due to small sample sizes in certain
# areas, there is still deviation even where there is
# many points. In addition, the variance of the data
# does not seem constant.

# Create residual plots
smoother <- smooth.spline(fit.value, stu.res)
plot(fit.value, stu.res)
abline(h=0, lty=2)
lines(smoother, col="red")
# While the trend and variability are the same, we can
# see that none of the studentized residuals are greater or
# less than 3 and -3, respectively.

# QQ-norm plot
qqnorm(res)
qqline(res)
# Looks very much normally distributed

# Partial residual plots
crPlots(linear.fit)
# I would consider taking the log of density as the values
# are scrunched together. Transforming it would make
# visualizing easier. The trends seem to break down at smaller
# values.

# Observe leverage values
lev <- hatvalues(linear.fit)
boxplot(lev)
plot(lev)

# Highlight points with high leverage
plot(Freedman$nonwhite, Freedman$population)
points(Freedman$nonwhite[lev>0.1], Freedman$population[lev>0.1], col="red")
# Notice that the red points are not the extreme
# values in the plot. This is because we are
# accounting for multiple variables.

# Calculate PRESS residuals and statistic
press.res <- res / (1-lev)
plot(press.res)

# Plot Cook's distance
n <- dim(Freedman)[1]
cook <- cooks.distance(linear.fit)
plot(cook)
abline(h=1, col="blue")
abline(h=4/n, col="red")
# We see quite a few highly influential points.

# Calculate DFFITS
dff <- dffits(linear.fit)
plot(dff)
abline(h=(2*sqrt(3/n)))
# Not any highly influential points, according to this.

# Plot DFBETAS
dfbetaPlots(linear.fit)
dfbetaPlots(linear.fit, id.n=3)
# Different cities influence the different coefficients.
# Quite  few of them are influential for at least one.

# While a linear regression seems to be correct, there seems to be
# model misspecification. Perhaps more predictors or higher orders
# of the existing predictors are needed.

##############
# NUMBER TWO #
##############

# Load ggplot2 package
require(ggplot2)

# Run the given code
data(diamonds)
n.obs <- dim(diamonds)[1]
big <- matrix(nrow=n.obs)
big[diamonds$carat < 1] <- 0
big[diamonds$carat >= 1] <- 1

# Fit a logistic regression
logit.fit <- glm(big ~ cut + color + clarity, data=diamonds, family="binomial")

# Save fitted values and residuals
fit.values <- fitted.values(logit.fit)
pear.res <- residuals(logit.fit, type="pearson")
dev.res <- residuals(logit.fit, type="deviance")
stu.dev.res <- rstudent(logit.fit, type="deviance")
stu.pear.res <- pear.res/sqrt(1-hatvalues(logit.fit))

# Residual plots
smoother <- smooth.spline(fit.values, stu.dev.res)
plot(fit.values, stu.dev.res)
abline(h=0, lty=2)
lines(smoother, col="red")
# Line doesn't look very constant at zero.
# Variability looks about the same across fitted values.

residualPlots(logit.fit)
# According to the bottom right plot, the trend does seem
# to be on zero. However, there are a lot of residual outliers
# for certain colors and clarities. Also, look at the centers
# of the distributions of the residuals for each predictor. 
# They do not seem to be near zero.

# Partial residual plots
crPlots(logit.fit)
# The medians tend to increase for color and decrease for clarity.
# However, it's hard to get anything out of this because the 
# predictors are categorical.

# Observe hat values
hat.values <- hatvalues(logit.fit)
plot(hat.values)
# Lots of points!

# Plot Cook's distance
cook <- cooks.distance(logit.fit)
n <- dim(dat)[1]
plot(cook)
abline(h=1, col="blue")
abline(h=4/n, col="red")
# No highly influential points

# Plot DFFITS
dff <- dffits(logit.fit)
plot(dff)
abline(h=(2/sqrt((3+1)/n)))
# Once again, no highly influential points

# Plot DFBETAS
dfbetaPlots(logit.fit)
# Try at your own risk! It took too long to run on my computer.

# It seems that there are no overly influential points. However,
# it seems that the model may struggle with predicting some of
# the points.

#############
# MODEL FIT #
#############

# Load the data
require(car)
data(Freedman)

# Remove missing values from data set
newFreedman <- Freedman[!is.na(Freedman$density),]

# Fit the three models
m1 <- glm(crime ~ nonwhite, data=newFreedman, family="gaussian")
m2 <- glm(crime ~ nonwhite + population, data=newFreedman, family="gaussian")
m3 <- glm(crime ~ nonwhite + population + density, data=newFreedman, family="gaussian")

# Calculate the AICs and BICs
AIC(m1)
AIC(m2)
AIC(m3)

BIC(m1)
BIC(m2)
BIC(m3)
# Both AIC and BIC are lowest for Model 2.

# Calculate likelihood ratio tests
lrtest(m1,m2)
lrtest(m2,m3)
# Model 1 fits the data significantly less well than Model
# 2 and Model 2 fits the data as well as Model 3. Therefore,
# LRT prefers Model 2.

# Compare effects of predictors using deviance
anova(m1, m2, test="Chisq")
anova(m2, m3, test="Chisq")
anova(m3, test="Chisq")
# Because we structured the models to add in one variable
# at a time, the third ANOVA produces the dame results as
# the first two. ANOVA points to model 2 fitting the model
# as well as model 3.

# Across all metrics, Model 2 is the preferred model. Usually,
# this does not happen - the different statistics can sometimes
# prefer different models. Use your best judgment and be sure 
# to explain why you chose the model you used.