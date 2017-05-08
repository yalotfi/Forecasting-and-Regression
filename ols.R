rm(list = ls())

# install.packages("readxl", dependencies = T)
library(readxl)

## Set working directory
# getwd()  # See where current directory is and change it
# setwd()

## Pre-Processing Data
df <- read_excel("PLEFinalProjectData.xlsx", "Purchasing Survey Data")
names(df) <- gsub(" ", ".", names(df))  # Replace spaces with '.' in column names

## Create formulas for linear model
createFormula <- function(df, target) {
  exclude <- names(df) %in% target  # logical vector for excluding target var from data.frame
  predictors <- paste(names(df)[!exclude], collapse = "+")  # Predictors of target
  return(as.formula(paste(target, " ~ ", predictors, sep = "")))  # Return formula for lm()
}
form.satis <- createFormula(df, "Satisfaction.Level")  # Create formula 1
form.usage <- createFormula(df, "Usage.Level")  # Create formula 2

## Build Regression Models
fit.satis <- lm(form.satis, data = df)
fit.usage <- lm(form.usage, data = df)

## Assess model on satisfaction levels
summary(fit.satis)
anova(fit.satis)
vif(fit.satis)
vcov(fit.satis)

## Assess model on usage level
summary(fit.usage)
anova(fit.usage)
vif(fit.usage)
vcov(fit.satis)

## Produce visual metrics
png("output/OLSPlotsSatis.png")
  layout(matrix(c(1,2,3,4),2,2))
    plot(fit.satis)
  layout(matrix(c(1,1)))
dev.off()
png("output/OLSPlotsUsage.png")
  layout(matrix(c(1,2,3,4),2,2))
    plot(fit.usage)
  layout(matrix(c(1,1)))
dev.off()
