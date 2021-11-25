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

Quan_fit05 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.05)
Quan_fit05
summary(Quan_fit05)

Quan_fit10 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.1)
Quan_fit10
summary(Quan_fit10)

Quan_fit15 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.15)
Quan_fit15
summary(Quan_fit15)

Quan_fit20 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.2)
Quan_fit20
summary(Quan_fit20)

Quan_fit25 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.25)
Quan_fit25
summary(Quan_fit25)

Quan_fit30 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.3)
Quan_fit30
summary(Quan_fit30)

Quan_fit35 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.35)
Quan_fit35
summary(Quan_fit35)

Quan_fit40 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.4)
Quan_fit40
summary(Quan_fit40)

Quan_fit45 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.45)
Quan_fit45
summary(Quan_fit45)

Quan_fit50 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.5)
Quan_fit50
summary(Quan_fit50)

Quan_fit55 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.55)
Quan_fit55
summary(Quan_fit55)

Quan_fit60 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.6)
Quan_fit60
summary(Quan_fit60)

Quan_fit65 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.65)
Quan_fit65
summary(Quan_fit65)

Quan_fit70 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.7)
Quan_fit70
summary(Quan_fit70)

Quan_fit75 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.75)
Quan_fit75
summary(Quan_fit75)

Quan_fit80 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.8)
Quan_fit80
summary(Quan_fit80)

Quan_fit85 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.85)
Quan_fit85
summary(Quan_fit85)

Quan_fit90 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.9)
Quan_fit90
summary(Quan_fit90)

Quan_fit95 <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = 0.95)
Quan_fit95
summary(Quan_fit95)

# Plot with Nasdaq

plot(Nifty50 ~ Nasdaq, data = stocks, main = "Plot", col = "blue")
abline(lm(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks), col = "red", lty = 2)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.05), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.1), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.15), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.2), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.25), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.3), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.35), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.4), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.45), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.5), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.55), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.6), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.65), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.7), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.75), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.8), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.85), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.9), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.95), lty = 3)

# Plot with SSE

plot(Nifty50 ~ SSE, data = stocks, main = "Plot", col = "blue")
abline(lm(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks), col = "red", lty = 2)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.05), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.1), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.15), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.2), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.25), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.3), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.35), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.4), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.45), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.5), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.55), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.6), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.65), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.7), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.75), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.8), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.85), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.9), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.95), lty = 3)



# Plot with LSE

plot(Nifty50 ~ LSE, data = stocks, main = "Plot", col = "blue")
abline(lm(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks), col = "red", lty = 2)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.05), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.1), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.15), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.2), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.25), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.3), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.35), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.4), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.45), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.5), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.55), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.6), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.65), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.7), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.75), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.8), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.85), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.9), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.95), lty = 3)



# Plot with Nikkei225

plot(Nifty50 ~ Nikkei225, data = stocks, main = "Plot", col = "blue")
abline(lm(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks), col = "red", lty = 2)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.05), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.1), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.15), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.2), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.25), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.3), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.35), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.4), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.45), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.5), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.55), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.6), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.65), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.7), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.75), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.8), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.85), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.9), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.95), lty = 3)



# Plot with Dow.Jones

plot(Nifty50 ~ Dow.Jones, data = stocks, main = "Plot", col = "blue")
abline(lm(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks), col = "red", lty = 2)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.05), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.1), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.15), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.2), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.25), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.3), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.35), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.4), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.45), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.5), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.55), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.6), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.65), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.7), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.75), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.8), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.85), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.9), lty = 3)
abline(rq(Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau=0.95), lty = 3)

#linear model

linear_fit <- lm( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks)
linear_fit
summary(linear_fit)

ggplot(data = stocks, aes(Nasdaq, Nifty50)) +
  geom_point() +
  geom_quantile(quantiles = qs)
qs <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95)

Quan_fit <- rq( Nifty50 ~ Nasdaq + SSE + LSE + Nikkei225 + Dow.Jones, data = stocks, tau = qs)
Quan_fit

ggplot(stocks, aes(Nasdaq, Nifty50)) + geom_point() + 
  geom_abline(intercept=coef(Quan_fit)[1], slope=coef(Quan_fit)[2])

plot(summary(Quan_fit))
plot(summary(Quan_fit), parm = 'SSE')


































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
