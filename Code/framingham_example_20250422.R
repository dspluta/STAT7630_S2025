library(ggplot2)
library(dplyr)

dat_raw <- read.csv("framingham.csv")
head(dat_raw)

# Objectives:
#   1a. Is there a significant association between current smoking status and 
#     ten year CHD status?
#   1b. Is there a significant associaiton between cigs/day and TenYearCHD, 
#         adjusting for sex, age, BMI, totChol, sysBP, diaBP?
#   2a. Is TenYearCHD significantly associated with BMI, adjusting for age, sex, 
# `     sysBP, diaBP?
#   2b. How does smoking status influence the BMI-TenYearCHD association? In particular,
#         is there a significant interaction?

# EDA
## Missing Data
sum(is.na(dat_raw))
lapply(dat_raw, function (x) sum(is.na(x)))
dat <- na.omit(dat_raw)

## Univariate
ggplot(dat) + 
  geom_histogram(aes(x = age))

ggplot(dat) + 
  geom_histogram(aes(x = cigsPerDay))

ggplot(dat) + 
  geom_histogram(aes(x = BMI))

## Bivariate

ggplot(dat) + 
  geom_boxplot(aes(color = currentSmoker, y = BMI, group = currentSmoker))

table(dat$currentSmoker, dat$TenYearCHD)

#### Modeling

# 1a. CHD ~ SMOKE

t.test(dat$currentSmoker, dat$TenYearCHD)

# 1b. CHD ~ Cigs/Day

fit <- lm(TenYearCHD ~ cigsPerDay, data = dat)
summary(fit)

fit <- lm(TenYearCHD ~ cigsPerDay + age + BMI + male, data = dat)
summary(fit)

plot(fit)

# 2. CHD ~ BMI

fit <- lm(TenYearCHD ~ BMI, data = dat)
summary(fit)

fit <- lm(TenYearCHD ~ BMI + age + male, data = dat)
summary(fit)

# 2b. CHD ~ BMI + BMI * SMOKE

fit <- lm(TenYearCHD ~ BMI + BMI:currentSmoker, data = dat)
summary(fit)

fit <- lm(TenYearCHD ~ BMI + BMI:currentSmoker + 
            age + male, data = dat)
summary(fit)

#

fit <- glm(TenYearCHD ~ cigsPerDay, family = "binomial", data = dat)
summary(fit)
