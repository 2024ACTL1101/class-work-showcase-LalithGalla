
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
# Initialise columns for daily return calculation 

df$dailyamd <- 0 
df$dailysp <- 0

# Calculate percentage change from current and previous day and update return

for (i in 2:nrow(df)) {
  df$dailyamd[i] <- (df$AMD[i]-df$AMD[i-1])/df$AMD[i-1]  
  df$dailysp[i] <- (df$GSPC[i]-df$GSPC[i-1])/df$GSPC[i-1]
}

# Ensure first day values are NA (instead of 0)

df$dailyamd[1] <- NA
df$dailysp[1] <- NA

```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
df$dailyrf <- 0 # Initialise daily risk-free rate column 

# Apply given formula to find daily risk-free rate in each row

for (i in 1:nrow(df)) {
  df$dailyrf[i] <- (1 + (df$RF[i] / 100))^(1/360) - 1
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
# Initialise columns excess AMD and S&P500 returns

df$excessamd <- 0
df$excessgspc <- 0

# Subtract risk-free rate from excess returns to find the excess return

for (i in 2:nrow(df)) {
  df$excessamd[i] <- df$dailyamd[i] - df$dailyrf[i]
  df$excessgspc[i] <- df$dailysp[i] - df$dailyrf[i]
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
# S&P500 excess as independent variable and AMD excess as dependent variable

amdsp_lm <- lm(excessamd ~ excessgspc, data = df)

summary(amdsp_lm) # display summary of the linear model
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**

The Beta value found from performing regression analysis is 1.57 (2 d.p.). Since the Beta value is greater than 1, this indicates that AMD is more volatile than the market. 

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
# Use ggplot package to obtain graph

ggplot(df, aes(x = excessgspc, y = excessamd)) +
  geom_point() +                                # Add points
  geom_smooth(method = "lm", color = "blue") + # Add regression line in blue
  labs(title = ("Excess Returns with Regression Line"), # Add title
       x = "S&P Excess Returns", # Add x-axis label
       y = "AMD Excess Returns") + # Add y-axis label
  theme_minimal()
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

The prediction interval will be [-0.49, 0.85]
```r
xf <- 0.133/252 - ((1 + 5/100)^(1/360) - 1) # Find daily xf from given formulas
n <- nrow(df) - 1 # Omit first row as this contains an NA value
xbar <- mean(df$excessgspc[2:nrow(df)]) # Find mean excess returns of S&P500

se <- sqrt(sum(residuals(amdsp_lm)^2)/(n-2)) # Apply formula for standard error
SSX <- sum((df$excessgspc - xbar)^2) 

sf <- se * sqrt(1 + 1/n + ((xf - xbar)^2 / SSX)) # Apply sf formula

t_value <- qt(1 - 0.1/2, df = n-2) # Find relevant t-value for 90% interval

expectedamd <- 0.05 + 1.57*(0.133-0.05) # CAPM formula for expected AMD return

LB <- expectedamd - (t_value * sf * sqrt(252)) # Lower bound with error
UB <- expectedamd + (t_value * sf * sqrt(252)) # Upper bound with error

# Round values to two decimal places 

roundedLB <- round(LB, 2) 
roundedUB <- round(UB, 2)

paste("The prediction interval is [", roundedLB, ",", roundedUB, "] to 2 d.p.")
```
