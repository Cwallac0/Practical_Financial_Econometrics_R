# Case Study II.1 from Practical Financial Econometrics 
library(tidyverse)
library(readxl)
library(magrittr)
library(modelr)

chp.2_path <- file.path("C:\\yourpath\\Case_Study_II.1")

vod_nok_dt <- read_excel(chp.2_path, sheet = "Voda Nokia Factor Model")

# The data used for factor models is on Voda-Nokia Factor Model tab
# Columns A through G are the raw data.  
# Code following makes returns that are in H through M

vod_nok_dt %<>% 
  mutate(Vodafone_rt       = log(Vodafone / lag(Vodafone)),
         Nokia_rt          = log(Nokia / lag(Nokia)),
         NYSE_Index_rt     = log(`NYSE Index` / lag(`NYSE Index`)),
         Communications_rt = log(Communications / lag(Communications)),
         Growth_rt         = log(Growth / lag(Growth)),
         Large_Cap_rt      = log(`Large Cap` / lag(`Large Cap`))) %>% 
  # Prefer names without the back ticks
  rename(NYSE_Index = `NYSE Index`,
         Large_Cap = `Large Cap`)

# Correlation
vod_nok_dt %>% 
  select(Vodafone_rt:Large_Cap_rt) %>% 
  na.omit() %>% 
  cor(.)

# Covariances
vod_nok_dt %>% 
  select(Vodafone_rt:Large_Cap_rt) %>% 
  na.omit() %>% 
  cov(.)

# Factor Models: Nokia
nok_lm_list <- list(
                Factor1 = as.formula(Nokia_rt ~ NYSE_Index_rt), 
                Factor2 = as.formula(Nokia_rt ~ NYSE_Index_rt + Communications_rt),
                Factor3 = as.formula(Nokia_rt ~ NYSE_Index_rt + Communications_rt + Growth_rt),
                Factor4 = as.formula(Nokia_rt ~ NYSE_Index_rt + Communications_rt + 
                                       Growth_rt + Large_Cap_rt)
)            

# Use purrr's map to perform regressions
nok_factor_models <- map(nok_lm_list, lm, data = vod_nok_dt)

# Examples of how to retrieve data from model list
summary(nok_factor_models$Factor4)
anova(nok_factor_models$Factor4)

vod_lm_list <- list(
  Factor1 = as.formula(Vodafone_rt ~ NYSE_Index_rt), 
  Factor2 = as.formula(Vodafone_rt ~ NYSE_Index_rt + Communications_rt),
  Factor3 = as.formula(Vodafone_rt ~ NYSE_Index_rt + Communications_rt + Growth_rt),
  Factor4 = as.formula(Vodafone_rt ~ NYSE_Index_rt + Communications_rt + 
                         Growth_rt + Large_Cap_rt)
)

# Use purrr's map to perform regressions
vod_factor_models <- map(vod_lm_list, lm, data = vod_nok_dt)

## Fig.II.1.4:  rebased to 100 data
vod_nok_dt %>% 
  transmute(
         Date = Date, 
         Vodafone = 100 * Vodafone / Vodafone[1],
         Nokia = 100 * Nokia / Nokia[1],
         NYSE_Index = 100 * NYSE_Index / NYSE_Index[1],
         Communications = 100 * Communications / Communications[1],
         Growth = 100 * Growth / Growth[1],
         Large_Cap = 100 * Large_Cap / Large_Cap[1]
         ) %>% 
  gather(., factors, values, -Date) %>% 
  ggplot(., aes(Date, values, col = factors)) + geom_line() +
  theme(legend.position = "bottom")

## Fig II.1.7: Portfolio Return
Vodafone <- 1
Nokia    <- 3

Vodafone_wt <- Vodafone / sum(Vodafone + Nokia)
Nokia_wt    <- Nokia / sum(Vodafone + Nokia)

vod_nok_dt %>% 
  select(Date, Vodafone_rt, Nokia_rt) %>% 
  mutate(port_ret = Vodafone_rt * Vodafone_wt + Nokia_rt * Nokia_wt) %>% 
  summarise(st_dev = sd(port_ret, na.rm = TRUE), vol = sqrt(250) * st_dev)


## Orthogonal Regressions
# Create PCAs
pcas <- prcomp(na.omit(vod_nok_dt[, c(10:13)]))
# Variance explained
summary(pcas)
# PCs
pcas$x

vod_nok_dt %<>%
  na.omit() %>% 
  cbind(., pcas$x) %>% tbl_df()

vod_pca_lm <- lm(Vodafone_rt ~ PC1 + PC2 + PC3 + PC4 - 1, data = vod_nok_dt) 
nok_pca_lm <- lm(Nokia_rt ~ PC1 + PC2 + PC3 + PC4 -1, data = vod_nok_dt)

# Net betas
pcas$rotation %*% vod_pca_lm$coefficients
pcas$rotation %*% nok_pca_lm$coefficients







