## Loading package pacman and use its function p_load for loading all required packages
library(pacman)
p_load(tibbletime, RcppRoll, timetk, tidyquant, tidyverse, broom, quantmod, estimatr, gt, gtsummary)

## BNO = United States Brent Oil Fund, ^OVX = CBOE Crude Brent Oil Volatility Index
tickers <- c("BNO", "^OVX")

## Collect Data from June 2010 until July 2024
prices <-
  getSymbols(tickers, src = "yahoo", from = "2010-06-02", to = "2024-07-01",
  auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(c("brent_crude_oil", "oil_vix"))

## Check imported data
head(prices)

## Calculate monthly log returns of Brent Crude Oil and its rolling 30 day Standard Deviation
brent_vol_rolling_vol <-
prices %>%
tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
mutate(brent_returns = (log(brent_crude_oil) - log(lag(brent_crude_oil)))) %>%
replace_na(list(brent_returns = 0)) %>%
mutate(brent_roll_30 = roll_sd(brent_returns, 30, fill = NA, align ="right"),
brent_roll_30 = (round((sqrt(252) * brent_roll_30 * 100), 2))) %>%
na.omit()

## Check on calculated log returns and rolling 30 day Standard Deviation  
head(brent_vol_rolling_vol)
  
## Plot of 30-Day Rolling Volatility and Crude Brent Oil Volatility Index
Oil_vix_versus_30_Day_Realized_Vol <- brent_vol_rolling_vol %>%
ggplot(aes(x = brent_roll_30, y = oil_vix)) +
geom_point(colour = "blue") +
geom_smooth(method = 'lm', se = FALSE, color = "red", linewidth = .5) +
ggtitle("Oil vix versus 30-Day Realized Vol") +
xlab("Realized vol preceding 30 trading days: 2010 - July 2024") +
ylab("oil_vix") +
scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
scale_x_continuous(labels = function(x){ paste0(x, "%") }) +
theme(plot.title = element_text(hjust = 0.5))

## Plot the graph
Oil_vix_versus_30_Day_Realized_Vol

## Save plot as pdf file
ggsave("Oil_vix_versus_30_Day_Realized_Vol.pdf")

## Plot of 30-Day Rolling Volatility and Crude Brent Oil Volatility Index with grouping by date
## to highlight the impact of the Russian invasion in Ukraine on the Brent Crude Oil Volatility
Oil_vix_versus_30_Day_Realized_Vol_shaded_by_date_Oct_2022_to_July_2024 <- brent_vol_rolling_vol %>%
  group_by(date) %>%
  filter(date >= "2022-10-01") %>%
  ggplot(aes(x = brent_roll_30, y = oil_vix, colour = date)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = "red", size = .5) +
  ggtitle("Oil vix versus 30-Day Realized Vol shaded by date: Oct 2022 - July 2024") +
  xlab("Realized vol preceding 30 trading days") +
  ylab("oil_vix") +
  scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
  scale_x_continuous(labels = function(x){ paste0(x, "%") }) +
  theme(plot.title = element_text(hjust = 0.5))

## Plot the graph
Oil_vix_versus_30_Day_Realized_Vol_shaded_by_date_Oct_2022_to_July_2024

## Save plot as pdf file
ggsave("Oil_vix_versus_30_Day_Realized_Vol_shaded_by_date_Oct_2022_to_July_2024.pdf")

## Plot of 30-Day Rolling Volatility and Crude Brent Oil Volatility Index shaded by date
Oil_vix_versus_30_Day_Realized_Vol_shaded_by_date <- brent_vol_rolling_vol %>%
  group_by(date) %>%
  ggplot(aes(x = brent_roll_30, y = oil_vix, colour = date)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = "red", size = .5) +
  ggtitle("Oil Vix versus 30-Day Realized Vol shaded by date") +
  xlab("Realized vol preceding 30 trading days") +
  ylab("oil_vix") +
  scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
  scale_x_continuous(labels = function(x){ paste0(x, "%") }) +
  theme(plot.title = element_text(hjust = 0.5))

## Plot the graph
Oil_vix_versus_30_Day_Realized_Vol_shaded_by_date

## Save plot as pdf file
ggsave("Oil_vix_versus_30_Day_Realized_Vol_shaded_by_date.pdf")

## Plot of Linear Regression of 30-Day Rolling Volatility and Crude Brent Oil Volatility Index for the year 2022
## to highlight the impact of the Russian invasion in Ukraine on the Brent Crude Oil Volatility
Oil_vix_versus_30_Day_Realized_Volatility_in_2022 <- brent_vol_rolling_vol %>%
  group_by(date) %>%
  filter(date >= "2021-12-31" & date <= "2023-01-01") %>%
  ggplot(aes(x = brent_roll_30, y = oil_vix, colour = date)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = "red", size = .5) +
  ggtitle("Oil_vix versus 30-Day Realized Vol: 2022") +
  xlab("Realized vol preceding 30 trading days") +
  ylab("vol_vix") +
  scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
  scale_x_continuous(labels = function(x){ paste0(x, "%") }) +
  theme(plot.title = element_text(hjust = 0.5))

## Plot the graph
Oil_vix_versus_30_Day_Realized_Volatility_in_2022

## Save plot as pdf file
ggsave("Oil_vix_versus_30_Day_Realized_Vol_in_2022.pdf")

## Plot of  Robust Linear Regression of 30-Day Rolling Volatility and Crude Brent Oil Volatility Index for the year 2022
## to highlight the impact of the Russian invasion in Ukraine on the Brent Crude Oil Volatility
Oil_vix_versus_30_Day_Realized_Volatility_in_2022_lm_robust <- brent_vol_rolling_vol %>%
  group_by(date) %>%
  filter(date >= "2021-12-31" & date <= "2023-01-01") %>%
  ggplot(aes(x = brent_roll_30, y = oil_vix, colour = date)) +
  geom_point() +
  stat_smooth(method = 'lm_robust') +
  theme_bw()+
  ggtitle("Oil_vix versus 30-Day Realized Vol: 2022") +
  xlab("Realized vol preceding 30 trading days") +
  ylab("vol_vix") +
  scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
  scale_x_continuous(labels = function(x){ paste0(x, "%") }) +
  theme(plot.title = element_text(hjust = 0.5))

## Plot the graph
Oil_vix_versus_30_Day_Realized_Volatility_in_2022_lm_robust

## Save plot as pdf file
ggsave("Oil_vix_versus_30_Day_Realized_Volatility_in_2022_lm_robust.pdf")

## Robust linear regression of the realized 30-Day rolling volatility of the United States Brent Oil Fund on the CBOE Crude Brent Oil Volatility Index
model_30 <- brent_vol_rolling_vol %>%
  lm_robust(oil_vix ~ brent_roll_30, data = .)

## Summary of regression result
summary(model_30)
tbl_regression(model_30)
glance(model_30)

## Plot of regression
model_30_lm <- brent_vol_rolling_vol %>% 
  ggplot(aes(brent_roll_30, oil_vix)) +
  geom_point() +
  stat_smooth(method = "lm_robust") +
  theme_bw()

## Plot the graph
model_30_lm

## Save plot as pdf file
ggsave("model_30_lm.pdf")

## Select R-Squared of the model
model_30_adj_r_squared <- glance(model_30) %>% 
                          select(adj.r.squared)

## Show the value of R-Squared
print(model_30_adj_r_squared)



