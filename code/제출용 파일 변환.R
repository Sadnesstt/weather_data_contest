# library & read data -----------------------------------------------------
library(tidyverse)
library(lubridate)
library(dplyr)

data = read_csv("test_final_tuning.csv")
data24 = data %>% filter(time == 24) %>%  mutate(X24H_TMA = mea_ddhr + hours(24)) %>% 
  rename("MEA_DDHR" = "mea_ddhr", "X24H_COND_LOC" = "cond_loc", 
         "X24H_COND_LOC_PROB" = "prob_cond_loc", "PLANT" = "plant", "LOC" = "location") %>% 
  select(MEA_DDHR, PLANT, LOC, X24H_TMA, X24H_COND_LOC, X24H_COND_LOC_PROB)




data48 = data %>% filter(time == 48) %>% mutate(X48H_TMA = mea_ddhr + hours(48)) %>% 
  rename("MEA_DDHR" = mea_ddhr, "X48H_COND_LOC" = cond_loc, 
         "X48H_COND_LOC_PROB" = prob_cond_loc, "PLANT" = "plant", "LOC" = "location") %>% 
  select(MEA_DDHR, PLANT, LOC, X48H_TMA, X48H_COND_LOC, X48H_COND_LOC_PROB)

submit <- left_join(data24, data48) %>% arrange(MEA_DDHR)
table(submit$X24H_COND_LOC)
table(submit$X48H_COND_LOC)

submit = submit %>% mutate(X24H_COND_LOC_PROB = 100 * X24H_COND_LOC_PROB,
                           X48H_COND_LOC_PROB = 100 * X48H_COND_LOC_PROB)
mean(submit$X24H_COND_LOC_PROB)
mean(submit$X48H_COND_LOC_PROB)
write_csv(submit, "203291.csv")
