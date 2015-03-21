library(foreign)
library(survey)

setwd("C:/Users/owner/Dropbox/Ph.D/Health Disparities Research")

dat <- read.xport("demo_e.xpt")

dat.design <- svydesign(
  id = dat$SDMVPSU,
  strata = dat$SDMVSTRA,
  data = dat,
  weights = dat$MEC6YR,
  nest = TRUE
  )

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