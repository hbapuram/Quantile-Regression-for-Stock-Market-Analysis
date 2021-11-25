# Installing the package 
#install.packages("dplyr") 

# Loading package 
library(dplyr) 

stocks <- read.csv("Stocks.csv") 
str(stocks)

# Installing Packages
#install.packages("quantreg")
#install.packages("ggplot2")
#install.packages("caret")

# Loading the packages
library(quantreg)
library(dplyr)
library(ggplot2)
library(caret)

# Model: Quantile Regression

qs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)

Quan_fit <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = qs)
Quan_fit

#linear model

linear_fit <- lm( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks)
linear_fit
summary(linear_fit)

#Summary Plots

plot(summary(Quan_fit))

#mid 50 analysis

qs1 <- c(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75)
Quan_fit_mid50 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = qs1)
Quan_fit_mid50

plot(summary(Quan_fit_mid50))

#Prediction and Box Plot of Results
test <- read.csv("test_data.csv")
pred_y <- predict.rq(Quan_fit, newdata = test)
actual_y <- test$Nifty50
pred_y

n50 <- test$Nifty50
p <- quantile(n50, qs)

boxplot(pred_y)
abline(lm(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks), col = "red", lty = 2)
points(actual_y, col = "black", pch = 19)
points(p, col = "blue", pch = 19)
legend("bottomright", "Quantiles of Actual y",pch = 19, col =  "blue")
title("Boxplots of Predicted y values, arranged Tau-wise")
summary(Quan_fit)

































qs <- 1:19/20
Quan_fit <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = qs)
Quan_fit


plot(summary(Quan_fit))


test <- read.csv("test_data.csv")

pred_y <- predict.rq(Quan_fit, newdata = test)

actual_y <- test$Nifty50

pred_y


n50 <- test$Nifty50
p <- quantile(n50, qs)



boxplot(pred_y)
abline(lm(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks), col = "red", lty = 2)
points(actual_y, col = "black", pch = 19)
points(p, col = "blue", pch = 19)
legend("bottomright", "Quantiles of Actual y",pch = 19, col =  "blue")
title("Boxplots of Predicted y values, arranged Tau-wise")
summary(Quan_fit)

fit0 <- rq(Nifty50~1,tau=qs,data=stocks)
fit1 <- rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones,tau=0.9,data=stocks)

rho <- function(u,tau=qs)u*(tau - (u < 0))
R1 <- 1 - abs((fit1$rho/fit0$rho))
R1
