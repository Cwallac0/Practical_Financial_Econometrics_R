
# Load tidyverse
library(tidyverse)


# Fig_II_3.1

df <- tbl_df(c(seq(10,200, 10))) %>%
  rename(col_a = value) %>% 
  mutate(col_b = qchisq(p = 0.025, col_a),
         col_c = qchisq(p = 0.975, col_a),
         col_d = col_a / col_c,
         col_e = col_a / col_b)


# 3.8
se_var_vol <- tbl_df(seq(0.7, 0.99, .01)) %>%
  rename(lambda = value) %>%
  mutate(se_var = sqrt(2 *(1 -lambda)/(1 + lambda)),
         se_vol = sqrt((1- lambda)/(2* (1 + lambda))))

se_var_vol %>%
  gather(se, percent, -lambda) %>%
  ggplot(., aes(lambda, percent, col = se)) + geom_line() +
  labs(y = "Estimated Standard Error (%)", title = "Fig_II_3.1")


