# Linear Regression

data(cars)

linear.fit <- lm(speed ~ dist, data=cars)
linear.fit
summary(linear.fit)
confint(linear.fit)

# Logistic Regression

data(diamonds)
n.obs <- dim(diamonds)[1]
big <- matrix(nrow=n.obs)
big[diamonds$carat < 1] <- 0
big[diamonds$carat >= 1] <- 1

logit.fit <- glm(big ~ cut, data=diamonds,
                 family="binomial"(link="logit"))
summary(logit.fit)
exp(logit.fit$coef)

# Generalized Linear Regression

logit.fit <- glm(big ~ cut, data=diamonds,
                 family="binomial"(link="logit"))
same.logit.fit <- glm(big ~ cut, data=diamonds,
                      family="binomial")
linear.fit <- glm(speed ~ dist, data=cars)
same.linear.fit <- glm(speed ~ dist, data=cars,
                       family="gaussian"(link="identity"))

# Generalized Estimating Equations

require(geepack)
wages <- read.table("http://www.ats.ucla.edu/stat/r/examples/alda/data/wages_pp.txt", header=T, sep=",")

gee.ind <- geeglm(lnw ~ exper, id=id, data=wages,
                  family="gaussian", corstr="independence")
summary(gee.ind)

gee.exch <- geeglm(lnw ~ exper, id=id, data=wages,
                   family="gaussian", corstr="exchangeable")
summary(gee.exch)

# Standard Errors and Hypothesis Testing

library(aod)

wald.test(b = coef(logit.fit), Sigma = vcov(logit.fit),
          Terms=2:5)

l <- cbind(0, 1, 0, 0, -1)
wald.test(b = coef(logit.fit), Sigma = vcov(logit.fit),
          L = l)

# Power Analysis

library(pwr)

pwr.anova.test(k=3, f=0.1, power=0.8, sig.level=0.05)

pwr.anova.test(k=3, f=0.1, power=0.5, sig.level=0.05)

# Prediction Models

library(rpart)

data(car.test.frame)
tree.fit <- rpart(Reliability~Country+Mileage+Type,
                  data=car.test.frame)

plot(tree.fit)
text(tree.fit)

require(LogicReg)
data(logreg.testdat)

logic.fit <- logreg(resp=logreg.testdat[,1],
                    bin=logreg.testdat[,2:21], type=2,
                    select=1, ntrees=5, nleaves=10)
logic.fit

require(randomForest)
data(imports85)

rf.fit <- randomForest(cityMpg ~ horsepower+numOfCylinders
                       +fuelType+bodyStyle, data=imports85,
                       mtry=4, importance=TRUE,
                       na.action=na.omit)
rf.fit