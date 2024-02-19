library(tidyverse)

dta <- readRDS("../samples_binned.rds") # A tibble: 12,000 × 10

# for sanity check 12000 is length(unique(dta$subj)) * length(unique(dta$face)) * length(unique(dta$angle))

dt <- dta %>% 
  select  (fame, ability, face, subj, angle, trial, rt, acc, sq, iaseq) %>% 
  arrange (fame, ability, face, subj, angle, trial)

C = dt %>% filter(ability == "control")
D = dt %>% filter(ability == "devprop")
S = dt %>% filter(ability == "super")

control_group = tibble()
for (fa in unique(C$face)){
  # isolate this face
  df = C %>% filter(face == fa)
  # make sequence id to make names for the transposed columns
  sqid <- paste0("s", str_pad(1:nrow(df), width=3, side='left', "0"))
  # separate off the iaseq
  iaseq = df$iaseq
  # combine into a tibble
  tmp = tibble(sqid=sqid, iaseq=iaseq)
  # transpose iaseq using sq as names
  transposed = tmp %>% pivot_wider(values_from = iaseq, names_from =  sqid)
  # put transposed iaseq as a row of column names in df
  df <- df %>% bind_cols(transposed)
  # give df$sq the padded values in sqid
  df$sq = sqid
  #
  # exclude a moving 4x4 window to forbid same-participant comparisons for the same face
  zerome = dim(C)[2] # zerome should be the number of cols that are not in the matrix of sequences
  for (r in seq(1, nrow(df)-3, 4)){
    for (k in 1:4){
      zerome = zerome + 1
      df[r+0, zerome] = 'q'
      df[r+1, zerome] = 'q'
      df[r+2, zerome] = 'q'
      df[r+3, zerome] = 'q'
    }
  }
  # exclude all same-angle comparisons (for the same face but different pps)
  # forbid A:A
  rows_with_A = seq(1,nrow(df),4)
  cols_with_A = seq(dim(C)[2]+1,ncol(df),4)
  for(row in rows_with_A){
    for(col in cols_with_A){
      df[row, col] = NA
    }
  }
  # forbid B:B
  rows_with_B = seq(2,nrow(df),4)
  cols_with_B = seq(dim(C)[2]+2,ncol(df),4)
  for(row in rows_with_B){
    for(col in cols_with_B){
      df[row, col] = NA
    }
  }
  # forbid C:C
  rows_with_C = seq(3,nrow(df),4)
  cols_with_C = seq(dim(C)[2]+3,ncol(df),4)
  for(row in rows_with_C){
    for(col in cols_with_C){
      df[row, col] = NA
    }
  }
  # forbid D:D
  rows_with_D = seq(4,nrow(df),4)
  cols_with_D = seq(dim(C)[2]+4,ncol(df),4)
  for(row in rows_with_D){
    for(col in cols_with_D){
      df[row, col] = NA
    }
  }
  
  control_group = bind_rows(control_group, df)
  write_csv(control_group, file="control_group_sequence_matrix.csv")
}

devprop_group = tibble()
for (fa in unique(D$face)){
  # isolate this face
  df = D %>% filter(face == fa)
  # make sequence id to make names for the transposed columns
  sqid <- paste0("s", str_pad(1:nrow(df), width=3, side='left', "0"))
  # separate off the iaseq
  iaseq = df$iaseq
  # combine into a tibble
  tmp = tibble(sqid=sqid, iaseq=iaseq)
  # transpose iaseq using sq as names
  transposed = tmp %>% pivot_wider(values_from = iaseq, names_from =  sqid)
  # put transposed iaseq as a row of column names in df
  df <- df %>% bind_cols(transposed)
  # give df$sq the padded values in sqid
  df$sq = sqid
  #
  # exclude a moving 4x4 window to forbid same-participant comparisons for the same face
  zerome = dim(D)[2] # zerome should be the number of cols that are not in the matrix of sequences
  for (r in seq(1, nrow(df)-3, 4)){
    for (k in 1:4){
      zerome = zerome + 1
      df[r+0, zerome] = 'q'
      df[r+1, zerome] = 'q'
      df[r+2, zerome] = 'q'
      df[r+3, zerome] = 'q'
    }
  }
  # exclude all same-angle comparisons (for the same face but different pps)
  # forbid A:A
  rows_with_A = seq(1,nrow(df),4)
  cols_with_A = seq(dim(D)[2]+1,ncol(df),4)
  for(row in rows_with_A){
    for(col in cols_with_A){
      df[row, col] = NA
    }
  }
  # forbid B:B
  rows_with_B = seq(2,nrow(df),4)
  cols_with_B = seq(dim(D)[2]+2,ncol(df),4)
  for(row in rows_with_B){
    for(col in cols_with_B){
      df[row, col] = NA
    }
  }
  # forbid C:C
  rows_with_C = seq(3,nrow(df),4)
  cols_with_C = seq(dim(D)[2]+3,ncol(df),4)
  for(row in rows_with_C){
    for(col in cols_with_C){
      df[row, col] = NA
    }
  }
  # forbid D:D
  rows_with_D = seq(4,nrow(df),4)
  cols_with_D = seq(dim(D)[2]+4,ncol(df),4)
  for(row in rows_with_D){
    for(col in cols_with_D){
      df[row, col] = NA
    }
  }
  
  devprop_group = bind_rows(devprop_group, df)
  write_csv(devprop_group, file="devprop_group_sequence_matrix.csv")
}

super_group = tibble()
for (fa in unique(S$face)){
  # isolate this face
  df = S %>% filter(face == fa)
  # make sequence id to make names for the transposed columns
  sqid <- paste0("s", str_pad(1:nrow(df), width=3, side='left', "0"))
  # separate off the iaseq
  iaseq = df$iaseq
  # combine into a tibble
  tmp = tibble(sqid=sqid, iaseq=iaseq)
  # transpose iaseq using sq as names
  transposed = tmp %>% pivot_wider(values_from = iaseq, names_from =  sqid)
  # put transposed iaseq as a row of column names in df
  df <- df %>% bind_cols(transposed)
  # give df$sq the padded values in sqid
  df$sq = sqid
  #
  # exclude a moving 4x4 window to forbid same-participant comparisons for the same face
  zerome = dim(S)[2] # zerome should be the number of cols that are not in the matrix of sequences
  for (r in seq(1, nrow(df)-3, 4)){
    for (k in 1:4){
      zerome = zerome + 1
      df[r+0, zerome] = 'q'
      df[r+1, zerome] = 'q'
      df[r+2, zerome] = 'q'
      df[r+3, zerome] = 'q'
    }
  }
  # exclude all same-angle comparisons (for the same face but different pps)
  # forbid A:A
  rows_with_A = seq(1,nrow(df),4)
  cols_with_A = seq(dim(S)[2]+1,ncol(df),4)
  for(row in rows_with_A){
    for(col in cols_with_A){
      df[row, col] = NA
    }
  }
  # forbid B:B
  rows_with_B = seq(2,nrow(df),4)
  cols_with_B = seq(dim(S)[2]+2,ncol(df),4)
  for(row in rows_with_B){
    for(col in cols_with_B){
      df[row, col] = NA
    }
  }
  # forbid C:C
  rows_with_C = seq(3,nrow(df),4)
  cols_with_C = seq(dim(S)[2]+3,ncol(df),4)
  for(row in rows_with_C){
    for(col in cols_with_C){
      df[row, col] = NA
    }
  }
  # forbid D:D
  rows_with_D = seq(4,nrow(df),4)
  cols_with_D = seq(dim(S)[2]+4,ncol(df),4)
  for(row in rows_with_D){
    for(col in cols_with_D){
      df[row, col] = NA
    }
  }
  
  super_group = bind_rows(super_group, df)
  write_csv(super_group, file="super_group_sequence_matrix.csv")
}

