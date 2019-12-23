# Examples_II.3=======================================

# EX_II.3.2-------------------------------------------
std_dev_rtns  <- 0.05
rtns_pa       <- 12
vol_iid       <- round(std_dev_rtns * sqrt(rtns_pa), 4)
auto_cor      <- 0.25
scale_fctr    <- rtns_pa + 2 * (auto_cor/(1-auto_cor)^2) * 
                 ((rtns_pa-1) * (1-auto_cor) - auto_cor*(1-auto_cor^(rtns_pa-1)))

vol_ar1       <- round(std_dev_rtns * sqrt(scale_fctr), 4)

# Ex_II.3.3 & 4---------------------------------------
# Asset Returns
asset1_vol <- 0.20
asset2_vol <- 0.10
asset3_vol <- 0.15

corr_1_2 <- 0.8
corr_1_3 <- 0.5
corr_2_3 <- 0.3

# Investments
inv_1 <- 1
inv_2 <- 2
inv_3 <- 3
                                      
# Weights
weights <- matrix(c(wgts_1 = inv_1/sum(inv_1, inv_2, inv_3),
                    wgts_2 = inv_2/sum(inv_1, inv_2, inv_3),
                    wgts_3 = inv_3/sum(inv_1, inv_2, inv_3)))

# Correlation Matrix
C <- matrix(c(1, corr_1_2, corr_1_3,
              corr_1_2, 1, corr_2_3,
              corr_1_3, corr_2_3, 1), nrow = 3)

# Volatility Matrix
D <- matrix(c(asset1_vol, 0, 0,
              0, asset2_vol, 0, 
              0, 0, asset3_vol), nrow = 3)

V <- D %*% C %*% D

port_var <- t(weights) %*% V %*% weights
port_vol <- sqrt(port_var)

# H_day calculations
h <- 10

D_h <- D/sqrt(250/h)

V_h <- D_h %*% C %*% D_h

V_alt <- V/(250/h)


# Ex_II.3.5 & 8---------------------------------------------------
# Get tidyquant to download data
library(tidyquant)

ftse <- tq_get("FTSE", get ="stock.prices", from = "2007-08-01")

ftse <- read.table('clipboard', header = FALSE, sep = "\t")

ftse %<>% tbl_df() %>% 
  mutate(d_log_rtn = log(V1/ lag(V1, 1)),
         sq_rtn = d_log_rtn^2) %>% 
  na.omit()

estimates <- ftse %>% 
  summarise(var_est = mean(sq_rtn),
            vol_est = sqrt(var_est) * sqrt(250))

# Critical values
cr_values_1 <- qchisq(p = 0.975, 10)
cr_values_2 <- qchisq(p = 0.025, 10)

# Variance interval
upper_bound_var <- 10 * estimates["var_est"]/cr_values_2
lower_bound_var <- estimates["var_est"] * 10/cr_values_1

# Volatility Interval
upper_bound_vol <- sqrt(upper_bound_var * 250)
lower_bound_vol <- sqrt(lower_bound_var * 250)


# EX_II.3.6------------------------------------------------------
ftse %<>% 
  mutate(mean_dev = d_log_rtn - mean(d_log_rtn),
         sq_mean_dev = mean_dev^2)

estimates2 <- ftse %>% 
  summarise(var_est = sum(sq_mean_dev)/9,
            vol_est = sqrt(var_est) * sqrt(250))

# EX_II.3.7-------------------------------------------------------
# Analogous to 3.6


# EX_II.3.9--------------------------------------------------------
conf_int   <- 0.95
volatility <- 0.20
samp_size  <- 30

chi_lower  <- qchisq((1 - conf_int)/2, samp_size)
chi_upper  <- qchisq((1 + conf_int)/2, samp_size)

var_upper  <- samp_size * volatility^2/chi_lower
var_lower  <- samp_size * volatility^2/chi_upper

vol_upper  <- sqrt(var_upper)
vol_lower  <- sqrt(var_lower)

# EX_II.3.10-----------------------------------------------------
# Analogous to 3.9

# EX_II.3.12-------------------------------------------------------
# Excellent package for financial time series analysis
library(tidyquant)
library(magrittr) # %<>% function

# Import indices from Yahoo Finance
# These dates won't correspond with book's excel
# Can import excel and save as objects below:
sp500 <- tq_get("^GSPC") 
FTSEMIB.MI <- tq_get("FTSEMIB.MI")

# Data cleaning: select close and create return columns
# return squared will be used for EWMA
sp500 %<>% 
  select(date, close) %>% 
  mutate(rtn = log(close / lag(close, 1)),
         rtn_sq = rtn^2) %>% 
  na.omit()

# Let's take opportunity to simplify the name
mib30 <- FTSEMIB.MI %>% 
  select(date, close) %>% 
  mutate(rtn = log(close / lag(close, 1)),
         rtn_sq = rtn^2) %>% 
  na.omit()


# Create volatility function to for data manipulations
vol_func <- function(x) sd(x) * sqrt(250)


# Add 30, 60, 90 day volatilities to dataframes
sp500 %<>%  
  tq_mutate(
    select     = rtn,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 30,
    align      = "right",
    by.column  = FALSE,
    FUN        = vol_func,
    # FUN args
    #na.rm      = TRUE
    col_rename = "vol_30"
  ) %>% 
  tq_mutate(
    select     = rtn,
    mutate_fun = rollapply,
    width      = 60,
    align      = "right",
    by.column  = FALSE,
    FUN        = vol_func,
    col_rename = "vol_60"
  ) %>% 
  tq_mutate(
    select     = rtn,
    mutate_fun = rollapply,
    width      = 90,
    align      = "right",
    FUN        = vol_func,
    col_rename = "vol_90"
  )


mib30 %<>% 
  tq_mutate(
    select     = rtn,
    mutate_fun = rollapply, 
    # rollapply args
    width      = 30,
    align      = "right",
    by.column  = FALSE,
    FUN        = vol_func,
    # FUN args
    #na.rm      = TRUE
    col_rename = "vol_30"
  ) %>% 
  tq_mutate(
    select     = rtn,
    mutate_fun = rollapply,
    width      = 60,
    align      = "right",
    by.column  = FALSE,
    FUN        = vol_func,
    col_rename = "vol_60"
  ) %>% 
  tq_mutate(
    select     = rtn,
    mutate_fun = rollapply,
    width      = 90,
    align      = "right",
    FUN        = vol_func,
    col_rename = "vol_90"
  )


# Graph Vols
sp500 %>% 
  select(-c(close, rtn, rtn_sq)) %>% 
  gather(vols, percent, -date) %>% 
  ggplot(., aes(date, percent, col = vols)) + geom_line() +
  labs(title = "SP500", y = "%", subtitle = "2009-2019", caption = "Yahoo Finance")

mib30 %>%
  # Potential data issues post 2019; vols increase dramatically
  filter(date < "2019-01-01") %>% 
  select(-c(close, rtn, rtn_sq)) %>% 
  gather(vols, percent, -date) %>% 
  ggplot(., aes(date, percent, col = vols)) + geom_line() +
  labs(title = "MIB", y = "%", subtitle = "2009-2018", caption = "Yahoo Finance")

# Create EWMA Variance column
library(MTS)
ewma_vol_sp  <- EWMAvol(sp500$rtn, lambda = 0.95)
ewma_vol_mib <- EWMAvol(mib30$rtn, lambda = 0.95)

# Graphing EWMA Vol
# Method used doesn't add to data to dataframe, but can be done easily
sp500 %>% 
  mutate(ewma_var = ewma_vol_sp$Sigma.t,
         ewma_vol = sqrt(ewma_var * 250)) %>%
  select(date, vol_60, ewma_vol) %>% 
  gather(vols, percent, -date) %>% 
  ggplot(., aes(date, percent, col = vols)) + geom_line() +
  labs(title = "SP 500: 60-day Vol & EWMA Vol", y = "%")

mib30 %>% 
  mutate(ewma_var = ewma_vol_mib$Sigma.t,
         ewma_vol = sqrt(ewma_var * 250)) %>%
  # Potential data issues post 2019; vols increase dramatically
  filter(date < "2019-01-01") %>%
  select(date, vol_60, ewma_vol) %>% 
  gather(vols, percent, -date) %>% 
  ggplot(., aes(date, percent, col = vols)) + geom_line() +
  labs(title = "MIB: 60-day Vol & EWMA Vol", y = "%")
  

