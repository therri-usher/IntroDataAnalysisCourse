# This is an example on how survey weighting can influence the
# conclusions of a model. Below, the variable dat.design contains
# the survey design for NHANES data. Please use this command if
# you are using NHANES for your data analysis project.

# Load the necessary packages
library(foreign)
library(survey)

setwd("C:/Users/owner/Dropbox/Ph.D/Health Disparities Research")

# Load the data
dat <- read.xport("demo_e.xpt")

# Define the sample design of the data
dat.design <- svydesign(
  id = dat$SDMVPSU,
  strata = dat$SDMVSTRA,
  data = dat,
  weights = dat$MEC6YR,
  nest = TRUE
  )

# These are the meanings of the variables used:
# gender = RIAGENDR
# age = RIDAGEYR
# race/ethnicity = RIDRETH1

# Fit a linear model of age with gender as a predictor without
# adjusting for survey weighting.
fit <- lm(RIDAGEYR ~ as.factor(RIAGENDR), data=dat)
summary(fit)

# Now, fit the same model, accounting for survey weights.
svyfit <- svyglm(RIDAGEYR ~ as.factor(RIAGENDR), data=dat, design=dat.design, family="gaussian")
summary(svyfit)
# Not a lot of difference...

# Now fit a linear model for age with race as a predictor,
# without accounting for survey design.
fit <- lm(RIDAGEYR ~ as.factor(RIDRETH1), data=dat)
summary(fit)

# Fit the same model, incorporating survey weights.
svyfit <- svyglm(RIDAGEYR ~ as.factor(RIDRETH1), data=dat, design=dat.design, family="gaussian")
summary(svyfit)
# While the estimates did not change much, the standard
# errors, and therefore some of the inference, did change.

setwd("C:/Users/owner/Dropbox/Ph.D/Health Disparities Research/HptnSegNHANES")

dat <- read.dta("Gaskin_master_lc.dta")

dat.design <- svydesign(
  id = dat$sdmvpsu,
  strata = dat$sdmvstra,
  data = dat,
  weights = dat$mec6yr,
  nest = TRUE
)

dat$hptn <- as.numeric(dat$hptn)
dat$hptn <- dat$hptn - 1

svyfit <- svyglm(hptn ~ riagendr, data=dat, family=binomial(link="logit"), design=dat.design)
fit <- glm(hptn ~ riagendr, data=dat, family=binomial(link="logit"))

svyby(~ridageyr, ~riagendr, design=dat.design, vartype="ci", svymean)