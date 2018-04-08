## DJIA Case Study II_2_ PCA Equity Factor Model
# The codes follow the convention of the case study in PFE
# R has functions such as princomp and prncomp that can create the eigenvectors, eigenvalues, &
# principal components in 1 call

library(tidyverse)
library(readxl)
library(stringr)
# Tidy regression output
library(broom)

# File path
djia_file <- file.path("C:\\Users\\CDub\\Documents\\R\\Examples\\MRA\\Chp2\\Case_Study_II.2_PCA_Equity_Factor_Model.xlsx")

# DJIA as of 2005. Use tidyquant for new DJIA
djia <- read_excel(djia_file, sheet = "DJIA Prices")

# Create dataframe of returns
djia_returns <- djia %>% 
  mutate_at(vars(AA:XON), function(x) log(x/lag(x, 1))) %>% 
  na.omit()

# Annualized average (bottom of DJIA returns tab)
djia_returns %>% 
  summarise_at(vars(AA:XON), function(x) 250 * mean(x))

# Annualize volatility (bottom of DJIA returns tab)
djia_returns %>% 
  summarise_at(vars(AA:XON), function(x) sqrt(250) * sd(x))

# Eigenvectors 
djia_ev <- eigen(cov(djia_returns[, -1]))$vectors * -1

# PCAs
djia_pca <- as.matrix(djia_returns[, -1]) %*% djia_ev

# Combine data
djia_rtn_pca <- cbind(djia_returns, PC = djia_pca) %>% tbl_df()

## PCA Factor Models
# Dependent variables
dep <- str_c(colnames(djia_rtn_pca[2:31]), "~", sep = " ")

# Independent variables
ind <- paste(colnames(djia_rtn_pca[32:36]), collapse = "+")

# Models
pca_models <- Map(function(x,y) lm(as.formula(paste(x, paste(y ,collapse="+"))), data=djia_rtn_pca), dep, ind)

# Tidyv version of above output
map(pca_models, tidy)

############################## Updated DJIA Data ############################################
# Tidyquant has function to download stocks of indices
# Use this to get updated / current DOW data
library(tidyquant)

# Get stock symbols for all 30 stocks
dow <- tq_index("DOW")

# Get daily data 
dow_get <- tq_get(dow$symbol,
                 get  = "stock.prices",
                 from = "2007-01-01",
                 to   = "2017-12-31")

# Convert data to wide format
dow_wide <- dow_get %>% 
  select(symbol, date, adjusted) %>% 
  spread(., symbol, adjusted)

#########################################################################################

########################## PCA Graphs ##################################################
library(factoextra)

# PCAs using prcomp function
djia_pca <- prcomp(cov(djia_returns[, -1]), scale = TRUE)

# Scree plot
fviz_eig(djia_pca)

# PCA plot
fviz_pca_ind(djia_pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
####################################################################################
