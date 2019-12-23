# Case Study_II.3=========================

# Import libraries
library(tidyquant) 

# Download US Treasury Data from FRED Database
# Using tidyquant function: tq_get
# Rename the price columns
m3  <- tq_get("DGS3MO", get = "economic.data") %>% rename(m3 = price)
m6  <- tq_get("DGS1MO", get = "economic.data") %>% rename(m6 = price)
y1  <- tq_get("DGS1", get = "economic.data") %>% rename(y1 = price)
y2  <- tq_get("DGS2", get = "economic.data") %>% rename(y2 = price)
y3  <- tq_get("DGS3", get = "economic.data") %>% rename(y3 = price)
y5  <- tq_get("DGS5", get = "economic.data") %>% rename(y5 = price)
y10 <- tq_get("DGS10", get = "economic.data") %>% rename(y10 = price)

# Combine into dataframe
tsy_list <- list(m3, m6, y1, y2, y3, y5, y10)
tsy_data <- reduce(tsy_list, left_join, by = "date")

# Check the data (2009-2019)
# Warning:  time period does not match examples in book
tsy_data
tail(tsy_data)

# Graph of data
tsy_data %>% 
  gather(treasuries, rates, -date) %>% 
  ggplot(., aes(date, rates, color = treasuries)) + geom_line() +
  labs(caption = "FRED Database")

# Daily Changes (bps)============================================
tsy_dc <- tsy_data %>% 
  mutate_if(is.numeric, function (x) (x - lag(x, 1)) * 100) %>% 
  na.omit()

# Just divided data into threes  
# Period A
period_a <- tsy_dc[c(1:880), ] 

period_a_vol <- map_df(period_a[, -1], function(x) sd(x) * sqrt(250))

cor_period_a <- cor(period_a[, -1])


# Period B
period_b <- tsy_dc[c(881: (881+880)), ]

period_b_vol <- map_df(period_b[, -1], function(x) sd(x) * sqrt(250))

cor_period_b <- cor(period_b[, -1])


# Period C
period_c <- tsy_dc[c((881+801) : nrow(tsy_dc)), ]

period_c_vol <- map_df(period_c[, -1], function(x) sd(x) * sqrt(250))

cor_period_c <- cor(period_c[, -1])



