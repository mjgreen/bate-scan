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

# values are averaged for each participant for each image
# compute rowwise mean similarity
mean_path_similarity_per_path <- dd %>% 
  rowwise() %>% 
  mutate(mean_sim=mean(c_across(s001:s045), na.rm=TRUE)) %>% 
  select(-c(s001:s045)) %>% 
  ungroup()

# then an overall average for the familiar and novel conditions per participant.
## this is the average of 80 values, being 20 famous faces in each of 4 angles.
# We get different values if we do the averaging in stages because there 
# are NAs in the data for individual angles (like if they didn't respond in time)
mp = mean_path_similarity_per_path %>% 
  group_by(subj, ability, fame) %>%
  summarise(mean_sim = mean(mean_sim, na.rm=TRUE)) %>% 
  ungroup()

# write analysis 3a scanmatch means
write_csv(mp, "analysis_3a_scanmatch_means.csv")



