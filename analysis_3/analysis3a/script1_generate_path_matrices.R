setwd("~/gits/bate-scan/analysis_3/analysis3a")
suppressPackageStartupMessages(library(tidyverse))

dt <- readRDS("../../dataset/binned_samples.rds") %>% 
  arrange(face, angle)

# separate matrices for each ability
C = dt %>% filter(ability == "control")
D = dt %>% filter(ability == "devprop")
S = dt %>% filter(ability == "super")

# Control Group
control_group = tibble()
for (fa in unique(C$face)){
  this_face = C %>% filter(face == fa)
  for (an in unique(this_face$angle)){
    df = this_face %>% filter(angle == an)
    # pad the numeric ia to make names for the transposed columns
    sqid <- paste0("s", str_pad(1:nrow(df), width=3, side='left', "0"))
    # separate off the iaseq
    iaseq = df$iaseq
    # combine into a tibble
    tmp = tibble(sqid=sqid, iaseq=iaseq)
    # transpose iaseq using sq as names
    transposed = tmp %>% pivot_wider(values_from = iaseq, names_from =  sqid)
    # put transposed iaseq as a row of df
    df <- df %>% bind_cols(transposed)
    # give df$sq the padded values in sqid
    df$sq = sqid
    # exclude the diagonal
    zerome = 10
    for (r in 1:nrow(df)){
      zerome = zerome + 1
      df[r, zerome] = 'x'
    }
    # recombine
    control_group = bind_rows(control_group, df)
  }
}
write_csv(control_group, "control_group_path_matrix.csv")

# Devprop Group
devprop_group = tibble()
for (fa in unique(C$face)){
  this_face = D %>% filter(face == fa)
  for (an in unique(this_face$angle)){
    df = this_face %>% filter(angle == an)
    # pad the numeric ia to make names for the transposed columns
    sqid <- paste0("s", str_pad(1:nrow(df), width=3, side='left', "0"))
    # separate off the iaseq
    iaseq = df$iaseq
    # combine into a tibble
    tmp = tibble(sqid=sqid, iaseq=iaseq)
    # transpose iaseq using sq as names
    transposed = tmp %>% pivot_wider(values_from = iaseq, names_from =  sqid)
    # put transposed iaseq as a row of df
    df <- df %>% bind_cols(transposed)
    # give df$sq the padded values in sqid
    df$sq = sqid
    # exclude the diagonal
    zerome = 10
    for (r in 1:nrow(df)){
      zerome = zerome + 1
      df[r, zerome] = 'x'
    }
    # recombine
    devprop_group = bind_rows(devprop_group, df)
  }
}
write_csv(devprop_group, "devprop_group_path_matrix.csv")

# Super Group
super_group = tibble()
for (fa in unique(C$face)){
  this_face = S %>% filter(face == fa)
  for (an in unique(this_face$angle)){
    df = this_face %>% filter(angle == an)
    # pad the numeric ia to make names for the transposed columns
    sqid <- paste0("s", str_pad(1:nrow(df), width=3, side='left', "0"))
    # separate off the iaseq
    iaseq = df$iaseq
    # combine into a tibble
    tmp = tibble(sqid=sqid, iaseq=iaseq)
    # transpose iaseq using sq as names
    transposed = tmp %>% pivot_wider(values_from = iaseq, names_from =  sqid)
    # put transposed iaseq as a row of df
    df <- df %>% bind_cols(transposed)
    # give df$sq the padded values in sqid
    df$sq = sqid
    # exclude the diagonal
    zerome = 10
    for (r in 1:nrow(df)){
      zerome = zerome + 1
      df[r, zerome] = 'x'
    }
    # recombine
    super_group = bind_rows(super_group, df)
  }
}
write_csv(super_group, "super_group_path_matrix.csv")
