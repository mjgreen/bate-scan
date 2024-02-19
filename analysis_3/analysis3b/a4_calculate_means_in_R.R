library(tidyverse)
options(dplyr.summarise.inform = FALSE)


control <- read_csv(
  "control_group_scanmatch_values_matrix.csv",
  na = "<undefined>", 
  show_col_types = FALSE
  ) %>% 
  mutate(image="diff") %>%
  select(-sq) %>% 
  relocate(image) %>% 
  rowwise() %>% 
  mutate(mean_sim=mean(c_across(s001:last_col()), na.rm=TRUE)) %>% 
  select(image:acc,mean_sim)

devprop <- read_csv(
  "devprop_group_scanmatch_values_matrix.csv",
  na = "<undefined>", 
  show_col_types = FALSE
  ) %>% 
  mutate(image="diff") %>% 
  select(-sq) %>% 
  relocate(image) %>% 
  rowwise() %>% 
  mutate(mean_sim=mean(c_across(s001:last_col()), na.rm=TRUE)) %>% 
  select(image:acc,mean_sim)

super <- read_csv(
  "super_group_scanmatch_values_matrix.csv",
  na = "<undefined>", 
  show_col_types = FALSE
  ) %>% 
  mutate(image="diff") %>% 
  select(-sq) %>% 
  relocate(image) %>% 
  rowwise() %>% 
  mutate(mean_sim=mean(c_across(s001:last_col()), na.rm=TRUE)) %>% 
  select(image:acc,mean_sim)


analysis_4_means = bind_rows(
  control, devprop, super
) 

write_csv(analysis_4_means, "analysis_4_means.csv")


