# Case_Studdy_II.1=====================================================

# Load libraries
library(tidyverse)
library(lubridate)
library(purrr)
library(readxl)

# Voda_Nokia Factor Model tab------------------------------
# Create file path  
files <- file.path("your_file_path")

# Import data using readxl function
data <- read_excel(files, sheet = "Voda_Nokia Factor Model")  

# Spreadsheet has calculations; below codes does them in tidyverse
data_rt <- data %>% 
  mutate_if(is.numeric, function(x) log(x/lag(x))) %>% 
  na.omit()

# corrplot makes nice correlation graphs
corrplot::corrplot(cor(data_rt[, -1]))
cov(data_rt[, -1])

# Create a list of regression formulas
nokia_regs_list <- list(
  mod1 = as.formula(Nokia ~ NYSE.Index),
  mod2 = as.formula(Nokia ~ NYSE.Index + Communications),
  mod3 = as.formula(Nokia ~ NYSE.Index + Communications + Growth),
  mod4 = as.formula(Nokia ~ NYSE.Index + Communications + Growth + Large.Cap)
)

vodafone_regs_list <- list(
  mod1 = as.formula(Vodafone ~ NYSE.Index),
  mod2 = as.formula(Vodafone ~ NYSE.Index + Communications),
  mod3 = as.formula(Vodafone ~ NYSE.Index + Communications + Growth),
  mod4 = as.formula(Vodafone ~ NYSE.Index + Communications + Growth + Large.Cap)
)

# Map function to quickly run the regressions
nokia_regs <- map(nokia_regs_list, lm, data_rt)
map(nokia_regs, summary)

vodafone_regs <- map(vodafone_regs_list, lm, data_rt)
map(vodafone_regs, summary)

# Fig_II.1.4 tab------------------------
# This tab rebases the prices to 100
data_rebased <- data %>% 
  mutate_if(is.numeric, function(x) 100 * (x / first(x)))

# Graphing rebased data
data_rebased %>% 
  gather(equities, price, -Date) %>% 
  ggplot(., aes(Date, price, color = equities)) + geom_line() +
  labs(title = "Rebased Equities", subtitle = "Fig_II.4 Graph", caption= "Data Supplied by Textbook")


# Fig II.1.7 
# Create the weight vectors
vodafone_wt <- 0.25
nokia_wt    <- 1- vodafone_wt

# Portfolio returns 
portfolio <- data_rt %>% 
  mutate(port_rtn = Vodafone * vodafone_wt + Nokia * nokia_wt)

# Use summarise function to get SD and Vol
# Can use pipe at end of above function to get it in argument
portfolio %>% 
  summarise(sd = sd(port_rtn), vol = sd(port_rtn) * sqrt(250))

# Orthogonal Regressions tab ---------------------------------
library(pls)

# Use this for supervised PCA
pcr_vodafone <- pcr(Vodafone ~ NYSE.Index + Communications + Growth + Large.Cap, 
                    data = data_rt)
summary(pcr_vodafone)
coef(pcr_vodafone)

# PCA 
pca <- prcomp(data_rt[, c(4:7)])
# Principal components
pca$x

# Combine with original data set
pca_data <- cbind.data.frame(data_rt, pca$x) %>% tbl_df()

# Vodafone
pca_vodafone <- lm(Vodafone ~ PC1 + PC2 + PC3 + PC4, data = pca_data)
summary(pca_vodafone)
# Multtiple R2
sqrt(summary(pca_vodafone)$adj.r.squared)

# Nokia
pca_nokia <- lm(Nokia ~ PC1 + PC2 + PC3 + PC4, data = pca_data)
summary(pca_nokia)
# Multtiple R2
sqrt(summary(pca_nokia)$adj.r.squared)
