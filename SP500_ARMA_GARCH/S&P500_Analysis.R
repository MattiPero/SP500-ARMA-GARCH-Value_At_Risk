
# Import S&P 500 data from Yahoo Finance

dati <- read.csv("^GSPC_yahoofin.csv", header=T)
Close<-ts(dati$Close)
library(ggplot2)
dati$Date <- as.Date(dati$Date)
ggplot(dati, aes(x = Date, y = Close)) +
  geom_line() +
  labs(
    title = "S&P 500 - Closing Prices",
    x = "Date",
    y = "Close"
  ) +
  theme_minimal()

# Apply Logarithmic Transformation

ln_Close <- log(Close)
ggplot(dati, aes(x = Date, y = ln_Close)) +
  geom_line() +
  labs(
    title = "ln(S&P 500 - Closing Prices)",
    x = "Date",
    y = "ln(Close)"
  ) +
  theme_minimal()

# Check for Stationarity

lag.plot(ln_Close, set.lags = 1:12, type = "p", do.lines=F)
par(mfrow = c(1,2))
acf(ln_Close, ylim = c(-1, 1))
pacf(ln_Close, ylim = c(-1, 1))

# Log Returns

dClose <- diff(ln_Close)
dati <- dati[-1, ] 
par(mfrow = c(1,1))
ggplot(dati, aes(x = Date, y = dClose)) +
  geom_line() +
  labs(
    title = "Log Returns",
    x = "Date",
    y = "returns"
  ) +
  theme_minimal()
par(mfrow=c(1,2))
acf(dClose,ylim=c(-1,1))
pacf(dClose,ylim=c(-1,1))

# Model Order Selection

ar(dClose)
arima(dClose, order = c(9,0,0))
modello_media9 = arima(dClose, order = c(9,0,0))

# Residual Diagnostics

residui9 <- residuals(modello_media9)
par(mfrow=c(1,2))
acf(residui9,ylim=c(-1,1))
pacf(residui9,ylim=c(-1,1))

par(mfrow = c(1,1))
ggplot(dati, aes(x = Date, y = residui9)) +
  geom_line() +
  labs(
    title = "Residuals",
    x = "Date",
    y = "residuals"
  ) +
  theme_minimal()

residui9_squared <- residui9^2
ggplot(dati, aes(x = Date, y = residui9_squared)) +
  geom_line() +
  labs(
    title = "Squared Residuals",
    x = "Date",
    y = "squared_residuals"
  ) +
  theme_minimal()

par(mfrow = c(1,2))
acf(residui9_squared,ylim=c(-1,1))
pacf(residui9_squared,ylim=c(-1,1))
ar(residui9_squared)

# ARIMA-GARCH Order Determination

library(fGarch)

garchFit(formula = ~arma(9,0) + garch(14,0), data=dClose, trace=F)
garchFit(formula = ~arma(1,0) + garch(4,0), data=dClose, trace=F)
garchFit(formula = ~arma(1,0) + garch(1,1), data=dClose, trace=F)
garchFit(formula = ~arma(1,1) + garch(1,1), data=dClose, trace=F)
garchFit(formula = ~arma(0,0) + garch(1,1), data=dClose, trace=F)

# Best Model Identification

modello <- garchFit(formula = ~arma(1, 0) + garch(1,1), data=dClose, trace=F)

# Standardized Residual Diagnostics

residui <- residuals(modello, standardize = TRUE)

par(mfrow = c(1,2))
acf(residui,ylim=c(-1,1))
pacf(residui,ylim=c(-1,1))

residui_quadrati <- residui^2
acf(residui_quadrati,ylim=c(-1,1))
pacf(residui_quadrati,ylim=c(-1,1))

Box.test(residui,lag=15,type="Ljung-Box",fitdf =2)

par(mfrow = c(1,1))
ggplot(dati, aes(x = Date, y = residui)) +
  geom_line() +
  labs(
    title = "Residuals",
    x = "Date",
    y = "residuals"
  ) +
  theme_minimal()

# Residual Normality Testing

hist(residui,
     prob=T,
     main="histogram of the standardized residuals",
     breaks = 30, 
     col="red",
     border = "white",
     ylim=c(0,.45))
curve(dnorm(x), add=T, col="blue", lwd=2,ylim=c(0,.45))

qqnorm(residui,
       main = "QQ-Plot: standardized residuals vs Normal",
       pch = 16,
       col = "darkblue",
       cex = 0.6)
qqline(residui,
       col = "red",
       lwd = 2)

library(tseries)
jarque.bera.test(residui)

# Now we try to assume a skewed t-student distribution

modello_student <- garchFit(formula = ~arma(1, 0) + garch(1,1), data=dClose, cond.dist = "sstd", trace=F)
summary(modello_student)
garchFit(formula = ~arma(1, 1) + garch(1,1), data=dClose, cond.dist = "sstd", trace=F)

# Residuals

residui_student <- residuals(modello_student, standardize = TRUE)

par(mfrow = c(1,2))
acf(residui_student,ylim=c(-1,1))
pacf(residui_student,ylim=c(-1,1))

residui_quadrati_st <- residui_student^2
acf(residui_quadrati_st,ylim=c(-1,1))
pacf(residui_quadrati_st,ylim=c(-1,1))

#Residuals vs SSTD density

parametri <- coef(modello_student)
co_skew  <- parametri["skew"]
co_shape <- parametri["shape"]

xgrid <- seq(min(residui_student),
             max(residui_student),
             length.out = 1000)
ygrid <- dsstd(xgrid,
               mean = 0,
               sd = 1,
               nu = co_shape,
               xi = co_skew)

par(mfrow = c(1,1))
hist(residui_student,
     probability = TRUE,
     breaks = 30,
     col = "red",
     border = "white",
     main = "Standardized Residuals vs SSTD Density",
     xlab = "Residuals",
     ylim = c(0, max(ygrid) * 1.1))
lines(xgrid, ygrid,
      col = "blue",
      lwd = 3)

# QQ-plot vs Skewed t-Student

p <- ppoints(length(residui_student))
q_teorici <- qsstd(p, mean = 0, sd = 1, nu = co_shape, xi = co_skew)

plot(q_teorici, sort(residui_student),
     xlab = "Theoretical Quantiles (SSTD)",
     ylab = "Empirical Quantiles",
     main = "QQ-Plot: Standardized Residuals vs Skewed-t",
     pch = 16, col = "darkblue", cex = 0.6)
abline(0, 1, col = "red", lwd = 2)

# prediction

pred_modello <- predict(modello_student, n.ahead =50)

par(mfrow=c(1,1))
dati$dClose <- c(dClose)

# Skewed-t quantiles

q_lower <- qsstd(0.025, mean = 0, sd = 1, nu = co_shape, xi = co_skew)
q_upper <- qsstd(0.975, mean = 0, sd = 1, nu = co_shape, xi = co_skew)

date_future <- seq(from = max(dati$Date) + 1, length.out = 50, by = "day")

df_prediction <- data.frame(
  Date = date_future,
  Valore = as.numeric(pred_modello$meanForecast),
  li = as.numeric(pred_modello$meanForecast) + q_lower * as.numeric(pred_modello$standardDeviation),
  ls = as.numeric(pred_modello$meanForecast) + q_upper * as.numeric(pred_modello$standardDeviation)
)

ggplot() +
  geom_line(data = tail(dati, 100), aes(x = Date, y = dClose), color = "black", linewidth = 0.7) +
  geom_line(data = df_prediction, aes(x = Date, y = Valore), color = "red", linewidth = 0.8) +
  geom_line(data = df_prediction, aes(x = Date, y = li), color = "blue", linetype = "dashed") +
  geom_line(data = df_prediction, aes(x = Date, y = ls), color = "blue", linetype = "dashed") +
  labs(
    title = "50-Day Forecast (95% CI skewed-t)",
    x = "Time",
    y = "Log Returns"
  ) + 
  theme_minimal()

volatilita <- modello_student@sigma.t
df_vol <- data.frame(
  Date = dati$Date,
  Volatilita = volatilita
)

ggplot(df_vol, aes(x = Date, y = Volatilita)) +
  geom_line(color = "darkred") +
  labs(title = "Estimated Conditional Volatility",
       x = "Date", y = "Volatility") +
  theme_minimal()

#Value at Risk

# model parameters
mu_t    <- fitted(modello_student)
sigma_t <- modello_student@sigma.t

# Skewed-t quantiles for 5% VaR
q_05 <- qsstd(0.05, mean = 0, sd = 1, nu = co_shape, xi = co_skew)

# Daily VaR (in log returns)
VaR_05 <- -(mu_t + sigma_t * q_05)

# Add to dataframe

dati$VaR_05 <- VaR_05

ggplot(dati, aes(x = Date)) +
  geom_line(aes(y = dClose), color = "black", linewidth = 0.5) +
  geom_line(aes(y = -VaR_05), color = "orange", linewidth = 0.6) +
  labs(
    title = "Returns and 5% Value at Risk",
    x = "Date",
    y = "Log Return"
  ) +
  theme_minimal()

#Backtesting

violations_05 <- sum(dClose < -VaR_05, na.rm = TRUE)
n <- length(dClose)
cat("VaR 5% Violations:", violations_05, "out of", n, "observations",
    "(", round(100 * violations_05 / n, 2), "% )\n")
