# S&P 500 Volatility Modeling and Risk Forecasting using ARMA-GARCH Models

This project investigates the dynamics of daily S&P 500 returns through time series analysis in R. Starting from historical closing prices downloaded from Yahoo Finance, the data are transformed into log returns and analyzed to identify an appropriate model for both the conditional mean and conditional volatility. Several ARMA-GARCH specifications are estimated and compared, leading to the selection of the best-performing model. The final model is then used to forecast future returns and volatility, estimate the 5% Value at Risk (VaR), and assess its predictive performance through backtesting. The project provides an example of how econometric models can be applied to financial risk measurement and forecasting.

## Dataset

The analysis uses historical daily closing prices of the S&P 500 index downloaded from Yahoo Finance.

- Dataset: `^GSPC_yahoofin.csv`
- Source: Yahoo Finance

The dataset contains several market variables, only the following are used in this analysis:

- `Date`: trading date.
- `Close`: daily closing price used to compute log prices and log returns.
