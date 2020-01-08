# Case Study II.5: Pairs of Practical Financial Econometrics

# libraries
library(tidyverse)
library(readxl)
library(magrittr)
library(janitor)

# File path  
data_path <- file.path("Your directory path/case_study.xlsx")

# Import data-------------------------------------------------------------------
futures <- read_excel(data_path, sheet = "Data")

# Let's practice using tidyverse to recreate the transformations 
futures_manual <- futures %>% select(Date, `Vstoxx Futures`, `Vdax Futures`) %>% 
  clean_names()

futures_manual %<>% 
  mutate(spread         = (vdax_futures - vstoxx_futures) * 10000,
         delta_vstoxx   = c(NA, diff(vstoxx_futures)),
         delta_vdax     = c(NA, diff(vdax_futures)),
         spread_1       = lag(spread),
         delta_vstoxx_1 = lag(delta_vstoxx),
         delta_vstoxx_2 = lag(delta_vstoxx, 2),
         delta_vdax_1   = lag(delta_vdax),
         delta_vdax_2   = lag(delta_vdax, 2)
     ) 

# ECM tab-----------------------------------------------------------------------
# Delta Vdax Equation
vdax_equation <- lm(delta_vdax ~ spread_1 + delta_vstoxx_1 + delta_vstoxx_2 + 
                      delta_vdax_1 + delta_vdax_2, data = futures_manual)

summary(vdax_equation)

# Delta Vstoxx Equation
vstoxx_equation <- lm(delta_vstoxx ~ spread_1 + delta_vstoxx_1 + delta_vstoxx_2 + 
                        delta_vdax_1 + delta_vdax_2, data = futures_manual)

summary(vstoxx_equation)

# ECM (tested down) tab---------------------------------------------------------
vdax_tested <- lm(delta_vdax ~ spread_1, data = futures_manual)

summary(vdax_tested)

vstoxx_tested <- lm(delta_vstoxx ~ spread_1 + delta_vstoxx_1 + delta_vstoxx_2 + 
                      delta_vdax, data = futures_manual) 

summary(vstoxx_tested)
