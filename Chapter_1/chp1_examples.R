# Examples II.1 =============================================
# Worked examples of first 3 examples.  


# Import Libraries
library(tidyquant)
library(readxl)
library(broom)
library(magrittr)

# Example 1.1--------------------------------------
# Use tidyquant to get data from Yahoo Finance
stocks <- c("^GSPC", "NWLI", "MSFT") %>% 
tq_get(get  = "stock.prices", 
       from = "2000-01-01",
       to   = "2007-08-27") %>%
  select(date, symbol, adjusted) %>% 
  spread(symbol, adjusted) %>% 
  tq_transmute(mutate_fun = to.weekly) %>% 
  rename(SPX = `^GSPC`)

# Create returns using dplyr
# Also check out return functions in tidyquant or quantmod
stocks_rtn <- stocks %>% 
  mutate_if(is.numeric, function(x) log(x / lag(x, 1))) %>% 
  na.omit()

# Regressions
nwl_fit  <- lm(NWLI ~ SPX, data = stocks_rtn)  
msft_fit <- lm(MSFT ~ SPX, data = stocks_rtn)  

# Specific risks
nwl_sr  <- glance(nwl_fit)["sigma"] * sqrt(52)
msft_sr <- glance(msft_fit)["sigma"] * sqrt(52)

# Portfolio characteristics
nwl_wt  <- 0.70
msft_wt <- 1 - nwl_wt

alpha     <- tidy(nwl_fit)[1, 2] * nwl_wt + tidy(msft_fit)[1, 2] * msft_wt

beta      <- tidy(nwl_fit)[2, 2] * nwl_wt + tidy(msft_fit)[2, 2] * msft_wt

spec_risk <- sqrt(nwl_sr^2 * nwl_wt^2 + msft_sr^2 * msft_wt^2)


# Example 1.2---------------------------------
stocks_2 <- c("^OEX", "AXP", "CSCO") %>%
  tq_get(get  = "stock.prices", 
         from = "2000-01-01",
         to   = "2007-12-31") %>%
  select(date, symbol, adjusted) %>% 
  spread(symbol, adjusted) %>% 
  #tq_transmute(mutate_fun = to.weekly) %>% 
  rename(SP100 = `^OEX`)
  
stocks_2_rtn <- stocks_2 %>% 
  mutate_if(is.numeric, function(x) log(x / lag(x, 1))) %>% 
  na.omit()

axp_wt  <- 0.60
csco_wt <- 1- axp_wt

stocks_2_rtn %<>% 
  mutate(port_rtn = AXP * axp_wt + CSCO * csco_wt)

axp_fit  <- lm(AXP ~ SP100, data = stocks_2_rtn)
csco_fit <- lm(CSCO ~ SP100, data = stocks_2_rtn)
port_fit <- lm(port_rtn ~ SP100, data = stocks_2_rtn)

# Table II.1.1
alpha <- data.frame(tidy(axp_fit)[1, 2], tidy(csco_fit)[1, 2], tidy(port_fit)[1, 2])
beta  <- data.frame(tidy(axp_fit)[2, 2], tidy(csco_fit)[2, 2], tidy(port_fit)[2, 2])
se    <- data.frame(glance(axp_fit)[, 3], glance(csco_fit)[, 3], glance(port_fit)[, 3])
sp_rk <- map_df(se, function(x) x * sqrt(250))


method_a <- sqrt(axp_wt^2 * glance(axp_fit)[, 3]^2 + csco_wt^2 * glance(csco_fit)[, 3]^2) * sqrt(250)
sp_vol <- sd(stocks_2_rtn$SP100) * sqrt(250)
ols_sys_risk <- tidy(port_fit)[2, 2] * sp_vol

cor(stocks_2_rtn[, -1])


# Example 1.3----------------------------------------------
beta_1   <- 0.8
beta_2   <- 1.2
rf_1_vol <- 0.15
rf_2_vol <- 0.20
rf_corr  <- -0.5
port_vol <- 0.25

cov_mat <- matrix(c(rf_1_vol^2, rf_corr * rf_2_vol * rf_1_vol,
                  rf_corr * rf_2_vol * rf_1_vol, rf_2_vol^2),  nrow = 2)

beta_mat <- matrix(c(beta_1, beta_2))


factor_var <- t(beta_mat) %*% cov_mat %*% beta_mat
factor_vol <- sqrt(factor_var)

spec_risk <- sqrt(port_vol^2 - factor_var)





