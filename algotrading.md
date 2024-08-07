
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm **WITH CUSTOM PERIOD** 
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.

Explanation of MY Code: 

The program loops through each relevant row (within my specified custom trading period) in the amd_df dataframe. On the first day, if the stock price has fallen compared to the previous day (even if the previous date is outside the specified period), it informs our decision to buy. For every day after the first buy, we compare the current price with the previous day's price. If the current price has fallen compared to the day before, we buy 100 shares at the current close price and update cost_proceeds and our accumulated_shares holding. Finally, on the last day, no matter what, we sell all of our holdings at the price on that day. 

```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA 
amd_df$accumulated_shares <- 0

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Initialize default start and end dates -- preparing for step 3 
default_start_date <- as.Date("2019-05-20")
default_end_date <- as.Date("2024-05-17")

# Initialize custom start and end dates 
start_date <- as.Date("2022-10-12")
end_date <- as.Date("2024-03-08")

# Loop through indexes ONLY from custom start date to end date 
for (i in which(amd_df$date == start_date):which(amd_df$date == end_date)) {
  current_price <- amd_df$close[i]
  
    # Used to check if stock price has fallen from previous day 
    if (start_date != default_start_date) {
        previous_price <- amd_df$close[i-1] 
    }

    # Check if it is the ending day where we choose to sell 
    if (amd_df$date[i] == end_date) {
        amd_df$trade_type[i] <- "sell"
        amd_df$costs_proceeds[i] <- (amd_df$close[i]) * accumulated_shares
        accumulated_shares <- 0 # reset accumulated shares to 0 after final sell
        amd_df$accumulated_shares[i:which(amd_df$date == default_end_date)] <- accumulated_shares
    } # Otherwise, if stock price has fallen OR previous price is 0, choose to buy
    else if (previous_price == 0 || previous_price > current_price) {
        amd_df$trade_type[i] <- "buy"
        amd_df$costs_proceeds[i] <- -1*amd_df$close[i]*share_size # money is LEAVING account
        accumulated_shares <- accumulated_shares + share_size 
        amd_df$accumulated_shares[i:nrow(amd_df)] <- accumulated_shares # updating every row after
    }

    previous_price <- current_price #update previous price when moving to next day
}
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years

This is done in step 2 already. 


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# This code executes AFTER step 2 loop has executed 

# Initialise profit/loss and invested capital as 0 before calculation

profit_loss <- 0
invested_capital <- 0 

# Loop through the finalised (edited) dataframe from custom start and end dates
for (k in which(amd_df$date == start_date):which(amd_df$date == end_date)) {
   
    # Whenever a 'buy' occurs, add the amount spent to invested cap. variable
    if (!is.na(amd_df$trade_type[k]) && amd_df$trade_type[k] == 'buy') {
        invested_capital <- invested_capital + amd_df$costs_proceeds[k] # Update invested cap. variable
    }

    # Whenever a 'buy' OR 'sell' occurs, add amount spent to profit/loss
    # This is using net profit = revenue - expenses 
    # NOTE(2): Invested capital is already negative
    if (!is.na(amd_df$costs_proceeds[k])) {
        profit_loss <- profit_loss + amd_df$costs_proceeds[k] # Update profit/loss variable
    }
}

# Apply ROI formula given in NOTE(1)
ROI <- (profit_loss/(-1*invested_capital))*100

# Display profit/loss, invested capital, and ROI metrics using paste()
paste("Profit/loss is: ", profit_loss, "Invested Capital is: ", invested_capital)
paste("ROI is: ", round(ROI, 2), "% (to 2 d.p.)")
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.

Explanation of MY Code:

I have chosen to implement a profit-taking strategy where I sell half of the holdings if the current price has increased compared to the previous day, and the percentage change between the current price and average purchase price is greater than 100%, i.e., double the average purchasing price. 

Overall, the code works by first mutating the amd_df dataframe to only include relevant columns. My code does NOT completely remove the rows not present in my specified period (2022-10-12 to 2024-03-08), but rather, just doesn't execute any actions on irrelevant dates (i.e., those outside my trading period). 

Overall Strategy/Code Logic: 

The code runs row by row and first checks if it is currently the end date. This takes priority as we MUST sell on the last day of trading. Otherwise, the program buys shares on the starting date and when the current price has fallen compared to the previous day. Throughout all days, the program updates the average_purchase_price column to continually reflect the average purchase price of the shares in the CURRENT portfolio (i.e., we don't keep track of shares that we are not currently holding). Using this continually updating average purchase price variable, the program checks if the price has risen by more than 100% of the average using the percentage change formula. If it has, we execute our 'profit-taking' manoeuvre by selling half our current holdings at the current_price. 

```r
amd_df <- read.csv("AMD.csv")

amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]

# Initialise dataframe columns for tracking 

amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA 
amd_df$accumulated_shares <- 0 

# Initialised NEW column to continually track avg_purchase_price of currently held shares
amd_df$avg_purchase_price <- 0 

previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Initialise start and end dates of given data set 
default_start_date <- as.Date("2019-05-20")
default_end_date <- as.Date("2024-05-17")

# Initialise custom trading period start and end dates
start_date <- as.Date("2022-10-12")
end_date <- as.Date("2024-03-08")


for (i in which(amd_df$date == start_date):which(amd_df$date == end_date)) {
    current_price <- amd_df$close[i]
    
    # First, check if we are at the end date, as it is priority to sell on the last day

    if (amd_df$date[i] == end_date) {
        amd_df$trade_type[i] <- 'sell'
        amd_df$costs_proceeds[i] <- current_price * accumulated_shares
        accumulated_shares <- 0 # reset accumulated shares to 0 on final sell
        amd_df$accumulated_shares[i:nrow(amd_df)] <- accumulated_shares
        amd_df$avg_purchase_price[i:nrow(amd_df)] <- 0
        break # ensures that program exits and ends on the final day  
    }
    
    # On the first day, automatically buy 
    if (previous_price == 0) {
        amd_df$trade_type[i] <- 'buy'
        # costs proceeds are negative since money is leaving account 
        amd_df$costs_proceeds[i] <- -1 * current_price * share_size 
        accumulated_shares <- accumulated_shares + share_size 
        # Update every proceeding row with new accumulated shares
        amd_df$accumulated_shares[i:nrow(amd_df)] <- accumulated_shares 
        #During first buy, close price is the average
        amd_df$avg_purchase_price[i:nrow(amd_df)] <- amd_df$close[i] 
    } # check if the stock price has risen 
    else if (current_price > previous_price) {
        # Now, check if risen by > 100% of average purchase price 
        # Apply standard percentage change formula
        # checking with average price BEFORE we choose to buy or sell, hence call [i-1]
        if (((current_price - amd_df$avg_purchase_price[i-1])/
             (amd_df$avg_purchase_price[i-1])) >= 1) { 
            
            amd_df$trade_type[i] <- 'sell_half'
            amd_df$costs_proceeds[i] <- current_price * (accumulated_shares/2) 
            accumulated_shares <- accumulated_shares/2 # sell half of the shares at  current price
            amd_df$accumulated_shares[i:nrow(amd_df)] <- accumulated_shares 
            
            # Update the avg_purchase_price using the WEIGHTED mean, which remains the same
            amd_df$avg_purchase_price[i:nrow(amd_df)] <-
              (amd_df$avg_purchase_price[i-1]*accumulated_shares)/accumulated_shares 
        }
    } # check if stock price has fallen compared to previous day, if TRUE, buy 
    else if (current_price < previous_price) { 
        amd_df$trade_type[i] <- 'buy'
        # negative costs proceeds since money leaves account 
        amd_df$costs_proceeds[i] <- -1 * current_price * share_size 
        accumulated_shares <- accumulated_shares + share_size
        amd_df$accumulated_shares[i:nrow(amd_df)] <- accumulated_shares
        
        # 100 shares are bought at a new price, so update the avg using the WEIGHTED mean. 
        # Apply weighted average formula:
        ## multiply average price and amount of shares that were previously held 
        ## Then add the current price multiplied by the amount of new shares bought, 
        ## Finally dividing this total value with the total number of shares held AFTER the purchase
        
        # Split numerator and denominator because otherwise, code line is too long to fit
        
        # Assign numerator 
        numerator <- (amd_df$avg_purchase_price[i-1]*amd_df$accumulated_shares[i-1]) + 
          (current_price*share_size) #weighted sum of share values
        
        # Assign denominator 
        denominator <- (accumulated_shares) #total accumulated shares 
        
        # Perform division for finding mean and update the variable
        amd_df$avg_purchase_price[i:nrow(amd_df)] <- numerator/denominator 
        
    }
    
    previous_price <- current_price #update previous price when moving to next day
}

# Step 3 logic, display new ROI after applying new strategy in trading period

invested_capital <- 0
profit_loss <- 0

for (k in which(amd_df$date == start_date):which(amd_df$date == end_date)) {
    if (!is.na(amd_df$trade_type[k]) && amd_df$trade_type[k] == 'buy') {
        invested_capital <- invested_capital + amd_df$costs_proceeds[k]
    }
    if (!is.na(amd_df$costs_proceeds[k])) {
        profit_loss <- profit_loss + amd_df$costs_proceeds[k]
    }
}

ROI <- (profit_loss/(-1*invested_capital))*100 # Convert invested capital to positive for ROI

paste("Profit/loss is: ", profit_loss, "| Invested Capital is: ", invested_capital)

paste("ROI is: ", round(ROI, 2), "% (to 2 d.p.)")
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.

My Answer:

- Net Profit and ROI both improved during this trading period 
- Relate your results to a relevant market event and explain why these outcomes may have occurred.
- Reiterating the output of step 4, net profit rose to **$1,792,803.99 and ROI** rose to **102.08%**
\vspace{1em}

Using a profit-taking strategy, I found that the improvements in profit and ROI that I obtained can be attributed to AMD’s engagement with the Artificial Intelligence (AI) processing industry, both creating and coinciding with investor enthusiasm regarding AI innovation. This pushed my ROI from **99.03%** to **102.08%**, representing a **$59,189.83** profit increase. 
\vspace{1em}

Specifically, on January 4th 2023, AMD launched the first iteration of their Instinct MI300 graphics processing unit (GPU), promising significant efficiency improvements for large language modelling (LLM) training. This coincides with the rise of user-facing LLMs such as ChatGPT, which reached 1 billion visitors in the following month (Feb 2023), showcasing rising user sentiment towards AI models. The rise in AMD shares the week following this announcement suggests that investors believe AMD’s involvement as an innovator in the AI hardware market could drive future growth. Using the profit-taking strategy during this period of upward growth allowed me to accumulate more profit at shorter time intervals than initially, therefore providing a better ROI. 
\vspace{1em}

To continue, on December 6th 2023, AMD presented their “Advancing AI” event, where CEO Lisa Su launched the MI300X (successor to the MI300). This new generation promised even greater AI and LLM training efficiency capabilities. During the same event, Lisa Su announced AMD’s partnerships with many major AI firms, notably when Microsoft announced plans to offer its Azure Cloud Computing services with an AMD platform, serving as an alternative to NVIDIA chips. During the following week, AMD shares rose by over 10%, fuelling a rising trajectory into early 2024. In particular, this event where AMD cemented their partnerships with other hugely successful and notable firms in the AI industry may have driven investors to believe that AMD is at the forefront of this emerging AI market space, driving future growth to new peaks. The use of my trading strategy during this period allowed me to accumulate more profit as AMD reached their peak trading price of USD$227.30 on the 8th of March 2024, providing a greater net profit, fuelling a more favourable ROI. 




