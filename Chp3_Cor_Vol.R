# Chp. 3  Volatility and COrrelation 
library(tidyverse)
library(readxl)
library(lubridate)
library(corrr)
library(magrittr)

# The dates are out of sequence in the accompanying spreadsheet
# Downloaded data from Federal Reserve:
#"https://www.federalreserve.gov/datadownload/Choose.aspx?rel=H15"

frb15_path <- file.path("C:\\Users\\CDub\\Documents\\R\\Examples\\MRA\\Chp3\\FRB_H15.xlsx")
frb15_path <- file.path("Your_path\\FRB_H15.xlsx")

# Import data 
# Skip first 5 rowws & manually set column types
frb15 <- read_excel(frb15_path, skip = 5, col_types = c("date", rep("numeric", 11)))

frb15 %<>% 
  rename(Date = `Time Period`,
         m1 = RIFLGFCM01_N.B,
         m3 = RIFLGFCM03_N.B,
         m6 = RIFLGFCM06_N.B,
         y1 = RIFLGFCY01_N.B,
         y2 = RIFLGFCY02_N.B,
         y3 = RIFLGFCY03_N.B,
         y5 = RIFLGFCY05_N.B,
         y7 = RIFLGFCY07_N.B,
         y10 = RIFLGFCY10_N.B,
         y20 = RIFLGFCY20_N.B,
         y30 = RIFLGFCY30_N.B) %>% 
  select(Date, m3:y5, y10)

# Graph of interest rates
frb15 %>% 
  filter(Date > "1982-01-01") %>% 
  gather(term, percent, -Date) %>% 
  ggplot(., aes(Date, percent, col = term)) + geom_line()

## Daily Changes tab
Daily_changes <- frb15 %>% 
  mutate_at(vars(-Date), function(x) (x - lag(x))*100) 

# Period A
Period_A2 <- Daily_changes %>% 
  filter(Date > "1991-01-01" & Date < "1995-12-29") %>% na.omit()

Period_A2 %>% 
  summarise_at(vars(-Date), function(x) sd(x) * sqrt(250))

Period_A2[, -1] %>% 
  correlate() 

# Period B
Period_B <- Daily_changes %>% 
  filter(Date > "1996-01-01" & Date < "1999-12-31") %>% na.omit()

Period_B %>% 
  summarise_at(vars(-Date), function(x) sd(x) * sqrt(250))

Period_B[, -1] %>% 
  correlate() 

# Period C 
Period_C <- Daily_changes %>% 
  filter(Date > "2000-01-01" & Date < "2005-12-31") %>% na.omit()

Period_C %>% 
  summarise_at(vars(-Date), function(x) sd(x) * sqrt(250))

Period_C[, -1] %>% 
  correlate()


#Relative Returns 
Returns_rel <- frb15 %>% na.omit() %>% 
  mutate_at(vars(-Date), fuction(x) log(x / lag(x))) 

frb15 %>% 
  filter(Date > "2006-01-01") %>% 
  mutate_at(vars(-Date), function(x) log(x/lag(x))) %>% 
  mutate_at(vars(-Date), function(x) ifelse(x %in% c(Inf, -Inf), NA, x)) %>% 
  na.omit() %>% 
  summarise_at(vars(-Date), function(x) sd(x) * sqrt(250))

  


  
  


 
  

