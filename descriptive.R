rm(list = ls())

# install.packages("readxl", dependencies = T)
# install.packages("purrr", dependencies = T)
# install.packages("tidyr", dependencies = T)
# install.packages("ggplot2", dependencies = T)
library(readxl)
library(purrr)
library(tidyr)
library(ggplot2)

## Set working directory
# getwd()  # See where current directory is and change it
# setwd()

## Pre-Processing both datasets
survey <- read_excel("PLEFinalProjectData.xlsx", "Purchasing Survey Data")
sales <- read_excel("PLEFinalProjectData.xlsx", "Tractor Sales")
names(survey) <- gsub(" ", ".", names(survey))  # Replace spaces with '.' in column names

## Helper Vars
nSurvey <- ncol(survey)
nSales <- ncol(sales)

## Survey Stat Summary
sink("output/surveySummary.txt")
  summary(survey[ ,2:nSurvey])
sink()

## Sales Stat Summary
sink("output/salesSummary.txt")
  summary(sales[2:nSales])
sink()

## Create Survey Histograms
png("output/histSurvey.png")
  survey[ ,2:nSurvey] %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot(aes(value)) +
      facet_wrap(~ key, scales = "free") +
      geom_histogram()
dev.off()

## Create Sales Histograms
png("output/histSales.png")
  sales[ ,2:nSales] %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot(aes(value)) +
      facet_wrap(~ key, scales = "free") +
      geom_histogram()
dev.off()
