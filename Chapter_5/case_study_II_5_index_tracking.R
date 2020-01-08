# Case Study II.5: Index Tracking of Practical Financial Econometrics

# libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(magrittr)
library(broom)
library(modelr)
library(scales)

# Import data from spreadsheet
data_path <- file.path("Your directory path/case_study.xlsx")

# Import data
sp500 <- read_excel(data_path, sheet = "Prices") 
  
# Clean data and convert columns to numeric
sp500 %<>% 
  slice(-1) %>% 
  mutate_if(is.character, ~ as.numeric(as.character(.)))

# Create log prices 
sp500_log <- sp500 %>% 
  mutate_if(is.numeric, ~ log(.))

# Manual Engle-Granger analysis-------------------------------------------------
# Book only uses first 16 stocks due to Excel limitations

sp500_reduce <- sp500_log %>% 
  select(Date, DJIA:IP)
colnames(sp500_reduce)

# Create linear model
model <- lm(DJIA ~ IBM +., data = sp500_reduce[, -1])
summary(model)
tidy(model)

# Remove intercept from sum
optimal_weights <- sum(model$coefficients[-1])

model$coefficients[-1]/optimal_weights

# Use residuals for Engle-Granger analysis
sp500_reduce <- add_residuals(sp500_reduce, model)

# Manipulate residuals with differences and lags
sp500_reduce %<>% 
  mutate(diff_res     = c(NA, diff(resid)),
         diff_res_lag = c(NA, lag(diff(resid))),
         lag_res      = lag(resid))

res_model <- lm(diff_res ~ diff_res_lag + lag_res, data = sp500_reduce)  
summary(res_model)

# Graph
ggplot(sp500_reduce, aes(Date, resid)) + geom_line() 

# Use R functions
# ca.jo function from the urca package
coint_function <- urca::ca.jo(sp500[, -1], ecdet = "const", type="eigen", K=2, 
                              spec="longrun" )

summary(coint_function)

################################################################################
# To get data from yahoo use the functions from tidyquant
################################################################################

# Create vector of stock symbols
symbols_index <- c("DJIA", "IBM", "MMM", "PG", "MSFT", "UTX", "JNJ", "MRK", 
                   "WMT", "CAT", "HD", "C", "GM", "KO", "MO", "DD", "IP")

# Download stocks using getSymbols
# Data must be transformed as above
dow_stocks <- getSymbols(symbols_index, 
                     src = 'yahoo',
                     from = "1990-01-02",
                     tp = "2001-12-31",
                     auto.assign = TRUE, 
                     warnings = FALSE) %>% 
  map(~Ad(get(.))) %>% 
  reduce(merge) %>% 
  `colnames<-` (symbols_index)



