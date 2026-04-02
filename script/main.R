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

# xts is a time series object, not a dataframe, so the syntax is different
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