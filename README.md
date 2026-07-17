# S&P 500 Volatility Modeling and Risk Forecasting using ARMA-GARCH Models

## Project Description

This project investigates the dynamics of daily S&P 500 returns through time series analysis in R. Starting from historical closing prices downloaded from Yahoo Finance, the data are transformed into log returns and analyzed to identify an appropriate model for both the conditional mean and conditional volatility. Several candidate models are estimated and compared, leading to the selection of the best-performing model. The final model is then used to forecast future returns and conditional volatility, estimate the 5% Value at Risk (VaR), and assess its predictive performance through backtesting. The project provides an example of how econometric models can be applied to financial risk measurement and forecasting.

## Dataset

The analysis is based on historical daily closing prices of the S&P 500 index downloaded from Yahoo Finance.

- **Dataset:** `^GSPC_yahoofin.csv`
- **Source:** Yahoo Finance
- **Frequency:** Daily

The dataset contains several market variables; however, only the following are used in this analysis:

- **Date:** Trading date.
- **Close:** Daily closing price used to compute log prices and log returns.

## Workflow

The analysis consists of the following stages:

1. Data Import and Visualization
2. Data Transformation
3. Stationarity Assessment
4. Mean Model Selection
5. Residual Diagnostics
6. Volatility Modeling
7. Model Diagnostics
8. Forecasting
9. Risk Measurement

## Results

The analysis identified an ARMA(1,0)-GARCH(1,1) model with skewed Student-t innovations as the best-performing specification among the models considered. Compared with the Gaussian assumption, the skewed Student-t distribution provides a better representation of the empirical distribution of S&P 500 returns, particularly by capturing heavy tails and providing a better fit to the negative tail of the empirical return distribution.

However, the residual diagnostics and VaR backtesting indicate that the skewed Student-t distribution does not fully capture the asymmetry between positive and negative returns. Although it represents an improvement over the Gaussian specification, some discrepancies remain, suggesting that more flexible conditional distributions or asymmetric volatility models could provide a more accurate description of financial return dynamics.

## Limitations

- The selected model assumes constant parameters over the sample period.
- The analysis is based solely on historical price data and does not include exogenous variables.
- Although the skewed Student-t distribution improves the modeling of heavy tails, it does not fully reproduce the observed asymmetry between positive and negative returns.
- Future work could investigate more flexible conditional distributions or asymmetric volatility models (e.g., EGARCH or GJR-GARCH) to improve risk estimation and forecasting performance.
