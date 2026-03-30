# Project:
# Comparative Analysis of Stock Performance, Volatility, 
# and Correlation Across Sectors

# Import packages
install.packages(c("tidyverse", "quantmod", "reshape2", "ggrepel", "viridis"))

library(tidyverse)
library(quantmod)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(viridis)

# 10 stocks with 3 sectors
# Apple Inc., Microsoft, NVIDIA for the tech sector
# BP, Shell plc, and ExxonMobile for the energy sector
# HSBC, Barclays, JPMorgan Chase, and GoldmanSachs for the finance sector

stocks <- c("AAPL", "MSFT", "NVDA",     # Tech
            "BP", "SHEL", "XOM",        # Energy
            "HSBC", "BCS", "JPM", "GS") # Finance

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
             "Finance", "Finance", "Finance", "Finance")
)

returns <- na.omit(ROC(prices, type = "discrete"))

cumulative_returns <- cumprod(1 + returns)

cumulative_long <- data.frame(date = index(cumulative_returns), 
                              coredata(cumulative_returns)) %>%
  pivot_longer(-date, names_to = "Stock", values_to = "Value") %>%
  left_join(sector, by = "Stock")

sector_performance <- cumulative_long %>%
  group_by(date, Sector) %>%
  summarise(Value = mean(Value), .groups = "drop")

ggplot(sector_performance, aes(x = date, y = Value, color = Sector)) +
  geom_line(size = 1.1) +
  labs(title = "Sector Performance Comparison", y = "Growth of £1 investment") +
  theme_minimal()
# Tech sector is showing a relatively steeper line

mean_returns <- apply(returns, 2, mean)

summary_table <- data.frame(
  Stocks = stocks,
  Sector = sector$Sector,
  Avg_Return = mean_returns
)

summary_table


# Section B: Risk and Volatility
volatility <- apply(returns, 2, sd)
volatility

summary_table$Volatility <- volatility
summary_table

returns_df <- data.frame(date = index(returns), coredata(returns)) %>%
  pivot_longer(-date, names_to = "Stock", values_to = "Return") %>%
  left_join(sector, by = "Stock")

ggplot(returns_df, aes(x = Stock, y = Return, fill = Sector)) +
  geom_boxplot() +
  labs(title = "Daily Returns Distribution by Stock") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Section C: Trends and Market Reactions
library(zoo)

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