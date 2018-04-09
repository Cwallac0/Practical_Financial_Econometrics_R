# Chp. 2 Forward CUrve PCA analysis
# The codes for short forward, short spot, and spot curves are analogous

# Libraries
library(tidyverse)
library(readxl)

# File path 
file_fwd <- file.path("Your_Path\\PCA_Fwd_Curve.xlsx")

# Load data
fwd <- read_excel(file_fwd, sheet = "Fwd")

# Create bps changes 
changes_bps <- fwd %>% 
  filter(Date > "2005-01-01") %>% 
  mutate_at(vars(`0.5 yr`:`25 yr`), function(x) x - lag(x)) %>%
  mutate_at(vars(`0.5 yr`:`25 yr`), function(x) x * 100) %>% 
  na.omit()

# Vol of changes
volatility <- map_df(changes_bps[, -1], function(x) sd(x) * sqrt(250))

# Inverse sd of changes
inv_stdev  <- map_df(changes_bps[, -1], function(x) 1/sd(x))

# Covariance tab
covariance <- cov(changes_bps[, -1])

# Correlation tab
correlations <- cor(changes_bps[, -1])

# Percentage variation explained
# Used only first 9 eigenvalues
covariance_perc_var <- eigen(covariance)$values[c(1:9)] / sum(eigen(covariance)$values)
correlation_perc_var <- eigen(correlations)$values[c(1:9)] / sum(eigen(correlations)$values)

# Cumulative variation explained
covariance_cum_var <- cumsum(eigen(covariance)$values[c(1:9)] / sum(eigen(covariance)$values))
correlation_cum_var <- cumsum(eigen(correlations)$values[c(1:9)] / sum(eigen(correlations)$values))

# Eigenvectors
covariance_eigenvectors  <- eigen(covariance, symmetric = FALSE)$vectors
correlation_eigenvectors <- eigen(correlations, symmetric = FALSE)$vectors 

# PCAs 
covariance_pca  <- princomp(changes_bps[, -1], cor = FALSE)$scores
correlation_pca <- princomp(changes_bps[, -1], cor = TRUE)$scores


####Graphs
# Must make changes for graphs, since names of columns become factors
library(stringr)
vol_names <- as.numeric(str_replace(colnames(volatility), "yr", ""))
colnames(volatility) <- vol_names

volatility %>% 
  gather(maturity, volatility) %>% 
  ggplot(., aes(sort(as.numeric(maturity)), volatility, group = 1)) + geom_line() +
  labs(x = "Maturity", y = "Volatility (Bps)", title = "Forward Curve Volatility")

covariance_eigenvectors %>% tbl_df() %>% 
  mutate(maturity = vol_names) %>% 
  select(V1:V3, maturity) %>% 
  gather(eigenvector, values, - maturity) %>% 
  ggplot(., aes(maturity, values, col = eigenvector)) + geom_line() +
  theme(legend.position = "bottom") +
  labs(title = "Eigenvectors", subtitle = "Covariance Matrix")

correlation_eigenvectors %>%  tbl_df() %>% 
  mutate(maturity = vol_names) %>% 
  select(V1:V3, maturity) %>% 
  gather(eigenvector, values, - maturity) %>% 
  ggplot(., aes(maturity, values, col = eigenvector)) + geom_line() +
  theme(legend.position = "bottom") +
  labs(title = "Eigenvectors", subtitle = "Correlation Matrix")


