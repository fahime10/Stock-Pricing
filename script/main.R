# Project:
# Comparative Analysis of Stock Performance, Volatility, 
# and Correlation Across Sectors

# Import packages
install.packages(c("tidyverse", "quantmod", "reshape2", 
                   "ggrepel", "viridis", "car", "Hmisc"))

library(tidyverse)
library(quantmod)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(viridis)
library(car)
library(Hmisc)

# 13 stocks with 4 sectors
# Apple Inc., Microsoft, NVIDIA for the tech sector
# BP, Shell plc, and ExxonMobile for the energy sector
# HSBC, Barclays, JPMorgan Chase, and GoldmanSachs for the finance sector
# Johnson & Johnson, Pfizer, UnitedHealth Group for the healthcare sector

stocks <- c("AAPL", "MSFT", "NVDA",     # Tech
            "BP", "SHEL", "XOM",        # Energy
            "HSBC", "BCS", "JPM", "GS", #Finance
            "JNJ", "PFE", "UNH") # Healthcare

getSymbols(stocks, 
           src = "yahoo",
           from = "2000-01-01",
           to = "2026-01-01")

View(AAPL)

prices <- do.call(merge, lapply(stocks, function(x) Ad(get(x))))

colnames(prices) <- stocks

# Section A: Performance of each sector and returns
sector <- data.frame(
  Stock = stocks,
  Sector = c("Tech", "Tech", "Tech",
             "Energy", "Energy", "Energy",
             "Finance", "Finance", "Finance", "Finance",
             "Healthcare", "Healthcare", "Healthcare")
)

returns <- na.omit(ROC(prices, type = "discrete"))

cumulative_returns <- cumprod(1 + returns)

# Cumulative returns in long format
cumulative_long <- data.frame(date = index(cumulative_returns), 
                              coredata(cumulative_returns)) %>%
  pivot_longer(cols = -date, names_to = "Stock", values_to = "Value") %>%
  left_join(sector, by = "Stock")

sum(is.na(cumulative_long$Sector))

sector_performance <- cumulative_long %>%
  group_by(date, Sector) %>%
  summarise(Value = mean(Value), .groups = "drop")

ggplot(sector_performance, aes(x = date, y = Value, color = Sector)) +
  geom_line(size = 1.1) +
  labs(title = "Sector Performance Comparison", y = "Growth of £1 investment") +
  theme_minimal()
# Tech sector is showing a relatively steeper line

# Compare two sectors at a time
unique_sectors <- unique(cumulative_long$Sector)

plot_sector_comparison <- function(data, sectors) {
  if(length(sectors) != 2) stop("Please provide exactly two sectors")
  
  if (!all(sectors %in% data$Sector)) stop("One or more sectors not found")
  
  sector_pair <- data %>%
    filter(Sector %in% sectors) %>%
    group_by(date, Sector) %>%
    summarise(Value = mean(Value), .groups = "drop")
  
  ggplot(sector_pair, aes(x = date, y = Value, color = Sector)) +
    geom_line(size = 1.1) +
    labs(title = paste(sectors[1], " vs ", sectors[2], " Performance"), y = "Growth of £1 investment") +
    theme_minimal()
}

# Generate the sector pairs
sector_pairs <- combn(unique_sectors, 2, simplify = FALSE)

lapply(sector_pairs, function(pair) {
  print(plot_sector_comparison(cumulative_long, pair))
})

returns_df <- data.frame(date = index(returns), coredata(returns)) %>%
  pivot_longer(-date, names_to = "Stock", values_to = "Return") %>%
  left_join(sector, by = "Stock")

ggplot(returns_df, aes(x = Stock, y = Return, fill = Sector)) +
  geom_boxplot() +
  labs(title = "Daily Returns Distribution by Stock") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mean returns
mean_returns <- apply(returns, 2, mean)

summary_table <- data.frame(
  Stocks = stocks,
  Sector = sector$Sector,
  Avg_Return = mean_returns
)

summary_table_sorted_by_returns <- summary_table %>%
  arrange(desc(Avg_Return))

summary_table_sorted_by_returns

# ANOVA testing
anova_result <- aov(Return ~ Sector, data = returns_df)
summary(anova_result)
# p-value < 0.05, so sectors are significantly different

# Tukey test
TukeyHSD(anova_result)

# Section B: Risk and Volatility
volatility <- apply(returns, 2, sd)
volatility

summary_table$Volatility <- volatility

summary_table_sorted_by_volatility <- summary_table %>%
  arrange(desc(Volatility))

summary_table_sorted_by_volatility

# Levene's test
leveneTest(Return ~ Sector, data = returns_df)
# p < 0.05, so volatility is significantly different across sectors

# Section C: Trends and Market Reactions
library(zoo)

# Rolling 30 refers to an analysis metric that is continuously updated to reflect
# the most recent 30 trading days of data. It is used to smooth out noise and 
# identify trends.
# The way it works is like a moving window. For example, rolling 30 on Day 30 
# refers to the average of days between 1-30, then rolling 30 on Day 31 refers
# the average of days between 2-31, and so on.
rolling_avg <- prices %>%
  na.omit() %>%
  as.data.frame() %>%
  mutate(Date = index(prices)) %>%
  pivot_longer(-Date, names_to = "Stock", values_to = "Price") %>%
  group_by(Stock) %>%
  mutate(Rolling30 = rollmean(Price, 30, fill = NA, align = "right")) %>%
  left_join(sector, by = "Stock")

ggplot(rolling_avg, aes(x = Date, y = Rolling30, color = Stock)) +
  geom_line(size = 1) +
  labs(title = "30-Day Rolling Average Price by Stock", y = "Price") +
  theme_minimal()

# Examine an event like COVID
returns_df$Period <- ifelse(
  returns_df$date < "2020-03-01", "Pre-COVID", "Post-Covid"
  )
# t-test
t_test_result <- t.test(Return ~ Period, data = returns_df)
t_test_result

# Section D: Correlation and Diversification
corr_matrix <- cor(returns)
View(corr_matrix)

corr_df <- melt(corr_matrix)

ggplot(corr_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +
  scale_fill_gradient2(low = "blue", 
                       mid = "white", 
                       high = "red", 
                       midpoint = 0.5) +
  labs(title = "Stock Correlation Matrix") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Test to see correlation significance among stocks
corr_results <- rcorr(as.matrix(returns))
corr_results$r
corr_results$P
# All pairwise collections were found to be statistically significant (resulting 
# in values closer to 0), indicating that relationships between stocks are 
# unlikely due to chance.
# However, the strength of the relationships varies.


stock_pairs <- combn(stocks, 2, simplify = FALSE)

correlation_calc <- function(data, stocks) {
  for (stock_pair in stocks) {
    corr_test <- suppressWarnings(
      cor.test(data[, stock_pair[1]], data[, stock_pair[2]])
      )
    cat(
      "Correlation of ", stock_pair[1], " and ", stock_pair[2], ":\n ", 
      " r = ", round(corr_test$estimate, 2),
      "| p-value = ", signif(corr_test$p.value, 2), "\n\n"
    )
  }
}

correlation_calc(returns, stock_pairs)


# Section E: Portfolio optimization
# The Sharpe ratio measures the performance of an investment compared to a 
# risk-free asset, after adjusting for its risk
returns_matrix <- na.omit(returns)

mean_returns <- colMeans(returns_matrix)

cov_matrix <- cov(returns_matrix)

# Monte Carlo simulation using 5000 as the number of simulations
set.seed(123)

num_portfolios <- 5000
num_assets <- ncol(returns_matrix)

results <- matrix(NA, nrow = num_portfolios, ncol = 3)
weights_list <- list()

for (i in 1:num_portfolios) {
  weights <- runif(num_assets)
  weights <- weights / sum(weights)
  
  weights_list[[i]] <- weights
  
  portfolio_return <- sum(weights * mean_returns)
  
  portfolio_risk <- sqrt(t(weights) %*% cov_matrix %*% weights)
  
  sharpe <- portfolio_return / portfolio_risk
  
  results[i, ] <- c(portfolio_return, portfolio_risk, sharpe)
}

results_df <- data.frame(
  Return = results[,1],
  Risk = results[,2],
  Sharpe = results[,3]
)

ggplot(results_df, aes(x = Risk, y = Return, color = Sharpe)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(title = "Efficient Frontier",
       x = "Risk (Volatility)",
       y = "Expected Return") +
  theme_minimal()

# Each dot represents a portfolio. The left represents low risk, and the top 
# represents high returns. The ideal portfolio is the one closest the top left

max_sharpe_index <- which.max(results_df$Sharpe)

optimal_weights <- weights_list[[max_sharpe_index]]

optimal_weights

optimal_portfolio <- data.frame(
  Stock = colnames(returns_matrix),
  Weight = optimal_weights
)

optimal_portfolio

# optimal_portfolio shows which stocks dominate the optimal mix

# weight refers to the proportion of the total investment in that stock.
# The weight is based on factors such as returns and risks, so higher weights 
# imply better risk-return trade-off of those criteria.
# For example, if you funded £1,000, then for AAPL, you would invest ~0.19, 
# which means investing £190 approximately. In this context, all the stocks' 
# weights add up to 1

optimal_point <- results_df[max_sharpe_index, ]

ggplot(results_df, aes(x = Risk, y = Return)) +
  geom_point(alpha = 0.4) +
  geom_point(data = optimal_point, 
             aes(x = Risk, y = Return),
             size = 4) +
  labs(title = "Efficient Frontier with Optimal Portfolio") +
  theme_minimal()

optimal_portfolio %>%
  left_join(sector, by = "Stock") %>%
  group_by(Sector) %>%
  summarise(Total_Weight = sum(Weight)) %>%
  arrange(desc(Total_Weight))

# The optimisation results allocate higher weights to technology and healthcare 
# stocks, suggesting that these sectors offer a more favourable risk-return 
# trade-off within the sample period


# Export to CSV
cumulative_long$date <- as.Date(cumulative_long$date)
sum(is.na(cumulative_long$Sector))

summary_table <- summary_table %>%
  mutate(Sharpe = Avg_Return / Volatility)

write.csv(cumulative_long, "data/cumulative_long.csv", row.names = FALSE)

write.csv(returns_df, "data/returns_df.csv", row.names = FALSE)

write.csv(optimal_portfolio, "data/optimal_portfolio.csv", row.names = FALSE)