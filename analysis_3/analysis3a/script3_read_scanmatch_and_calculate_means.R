setwd("/home/matt/gits/bate-scan/analysis_3/analysis3a/")
library(tidyverse)

dd = tibble()
for (group in c("control", "devprop", "super")){
  filename = paste0(group, "_group_scanmatch_values_matrix.csv")
  contents = vroom::vroom(filename, na = c("", "NA", ".", "<undefined>"), show_col_types = FALSE)
  dd <- dd %>% bind_rows(contents)
}
dd <- dd %>% mutate(
  subj=as_factor(subj), ability=as_factor(ability), face=as_factor(face), fame=as_factor(fame), angle=as_factor(angle)
)

write_csv(dd, "all_groups_scanmatch_values_matrix.csv")


