library(ggplot2)
library(dplyr)

dat_raw <- read.csv("STAT7630_S2024/STAT7630_S2024-main/Data/framingham.csv")
head(dat)

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
## Univariate

ggplot(dat) + 
  geom_histogram(aes(x = age))

ggplot(dat) + 
  geom_histogram(aes(x = cigsPerDay))

ggplot(dat) + 
  geom_histogram(aes(x = BMI))

sum(is.na(dat))
lapply(dat, function (x) sum(is.na(x)))

dat <- na.omit(dat)

## Bivariate

ggplot(dat) + 
  geom_boxplot(aes(color = currentSmoker, y = BMI, group = currentSmoker))

