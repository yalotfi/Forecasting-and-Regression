rm(list = ls())

# install.packages("readxl", dependencies = T)
# install.packages("forecast", dependencies = T)
library(readxl)
library(forecast)

df <- read_excel("PLEFinalProjectData.xlsx", "Tractor Sales")
minDate <- as.numeric(c(min(substr(df$Month, 1, 4)), 1))   # (2010, 1)
maxDate <- as.numeric(c(max(substr(df$Month, 1, 4)), 12))  # (2014, 12)

## Create Time-Series Object Matrix: (60, 6)
data.ts <- ts(data = df[ ,2:7]
              , start = minDate
              , end = maxDate
              , frequency = 12)
rm(minDate, maxDate)

## Visualize Time-Series
png("output/seriesPlots.png")
  plot(data.ts, main = "Time Series By Region")  # Plot time series sequences
dev.off()

## Seasonal Decomposition Charts
for (i in 1:ncol(data.ts)) {
  png(filename = paste("output/decompCharts"
                       , colnames(data.ts)[i]
                       , ".png"
                       , sep = ""))
  decomps <- stl(data.ts[ ,i], s.window = "period")
  chartTitle <- paste("Region: ", colnames(data.ts)[i], sep = "")
  plot(decomps, main = chartTitle)
  dev.off()
}
rm(i, decomps, chartTitle)

## Use Holt-Winters for Forecasts on Level, Trend, and Seasonality
model.NA <- hw(log(data.ts[, "NA"]), initial = "optimal", h = 60)
model.Pac <- hw(data.ts[, "Pac"], initial = "optimal", h = 60)
model.World <- hw(log(data.ts[, "World"]), initial = "optimal", h = 60)

## Use Double-Exponential Smoothing fore Forecasts on Level and Seasonality
model.Eur <- hw(data.ts[, "Eur"], initial = "optimal", h = 60, gamma = NULL)
model.SA <- hw(data.ts[, "SA"], initial = "optimal", h = 60, gamma = NULL)
model.China <- hw(data.ts[, "China"], initial = "optimal", h = 60, gamma = NULL)

# Store accuracy metrics for each model
accuracy.matrix <- t(rbind(accuracy(model.NA),
                           accuracy(model.Pac),
                           accuracy(model.World),
                           accuracy(model.Eur),
                           accuracy(model.SA),
                           accuracy(model.China)))
colnames(accuracy.matrix) <- c("NA", "Pac", "World", "Eur", "SA", "China")

# Plot Forecasts
png("output/FinalForecasts.png", width = 900, height = 600)
  layout(matrix(c(1,2,3,4,5,6),2,3))
    plot(model.NA, main = "NA: Holt-Winters'")
    plot(model.Pac, main = "Pac: Holt-Winters'")
    plot(model.SA, main = "SA: Double-Smoothing")
    plot(model.China, main = "China: Double-Smoothing")
    plot(model.Eur, main = "Eur: Double-Smoothing")
    plot(model.World, main = "World: Holt-Winters'")
  layout(matrix(c(1,1)))
dev.off()


# Plot MAE Errors by Region
png("output/MAE.png")
  barplot(accuracy.matrix["MAE", ]
          , main = "MAE Errors by Region"
          , xlab = "Region", ylab = "MAE")
dev.off()
