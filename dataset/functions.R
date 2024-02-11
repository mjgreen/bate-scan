suppressPackageStartupMessages(library(tidyverse))

remote_file_name     = "SAMPLES2.TXT"
local_file_name      = "dataset/samples_raw.txt"
compressed_file_name = "dataset/samples_compressed.rds"
binned_file_name     = "dataset/binned_samples.rds"

read_raw_file = function(
    local_file=local_file_name
    ){
  text_samples =
  vroom::vroom(
    file = local_file_name, na = c("", "NA","."), delim = "\t",
    col_select = c(Session_Name_, TRIAL_INDEX, trial, key_press, 
                   RT, TIMESTAMP, AVERAGE_INTEREST_AREA_LABEL),
    col_types = cols_only(Session_Name_ = "c", TRIAL_INDEX = "d", 
                          trial="c", key_press="c", RT="d", 
                          TIMESTAMP="d", AVERAGE_INTEREST_AREA_LABEL="c")
  )
  return(text_samples)
}

test_download = function(
    local_file=local_file_name  
  ){
  text_samples <- read_raw_file()
  message(paste0(
    "Number of rows in raw data should be: 59400000\n",
    "Number of rows in download is:      : ", nrow(text_samples)))
  if(nrow(text_samples) != 59400000){message("File is BAD")}
  if(nrow(text_samples) == 59400000){message("File is GOOD")}
  return(nrow(text_samples))
}

download_raw_samples = function(
    remote_file = remote_file_name,
    local_file = local_file_name,
    checking = TRUE
    ){
  
  while(checking == TRUE) {
    
    # If the file doesn't exist then go get it
    
    if(!file.exists(local_file_name)){
      odb <- get_business_onedrive()
      odb$download_file(src=remote_file_name, dest=local_file_name)
    }
    
    # If the file does exist check it for goodness
    
    if(file.exists(local_file_name)){
      message("File exists, checking if extant file is good")
      num_rows_downloaded = test_download(local_file=local_file)
    }
    
    # If the file is bad, remove it
    
    if(num_rows_downloaded != 59400000){
      file.remove(local_file_name)
    }
    
    # if the file is good exit the while loop
    
    if(num_rows_downloaded == 59400000){
      checking = FALSE
      }
  }
}


compress_raw_samples = function(
    local_file = local_file_name,
    compressed_file = compressed_file_name,
    force=FALSE){
  if(file.exists(compressed_file) & force==FALSE){
    return(message("compressed file exists, not compressing"))
  }
  message("reading samples from text")
  compressed_samples <- read_raw_file()
  message("compressing samples")
  compressed_samples <- compressed_samples %>% 
    dplyr::rename(subj="Session_Name_", stim="trial", trial="TRIAL_INDEX", 
                  resp="key_press", rt="RT", time="TIMESTAMP", 
                  ia="AVERAGE_INTEREST_AREA_LABEL") 
  message("saving compressed samples")
  saveRDS(compressed_samples, compressed_file)
  message("saving compressed samples is complete")
}


make_binned_sequences = function(
    verbose=FALSE,
    compressed_file = compressed_file_name,
    binned_file = binned_file_name
    ){
  if(file.exists(binned_file)){
    return(message("binned sequence file exists, not binning"))
  } 
  message("start making binned sequences")
  message("\tread compressed samples")
  dat <- readRDS(compressed_file) %>% 
    # 59,400,000 rows
    group_by(subj, trial) %>% 
    mutate(time = time-time[1]) %>%
    ungroup() %>% 
    # restrict to samples before the key-press where they respond
    filter(time < rt) %>% 
    # 18,613,098 rows
    # correct stim identifiers
    ungroup() %>% 
    mutate(
      marker_fame  = str_sub(stim,1,1),
      marker_ident = str_sub(stim,2,3),
      marker_angle = str_sub(stim,4,4)
    ) %>% 
    mutate(
      marker_angle=case_when(
        (marker_fame == "H" & marker_angle =="A") ~ "A" ,
        (marker_fame == "H" & marker_angle =="B") ~ "B" ,
        (marker_fame == "L" & marker_angle =="A") ~ "C" ,
        (marker_fame == "L" & marker_angle =="B") ~ "D" ,
        # make sure non-matching get original value
        .default = marker_angle
      )
    ) %>% 
    mutate(
      marker_fame = case_when(
        marker_fame %in% c("H","L") ~ "F", # famous
        marker_fame %in% c("D")     ~ "N", # novel
        .default=marker_fame
      )
    ) %>% 
    dplyr::rename(
      stim_orig="stim"
    ) %>% 
    mutate(
      face=paste0(marker_fame, marker_ident),
      stim=paste0(marker_fame, marker_ident, marker_angle)
    ) %>% 
    dplyr::rename(
      ident="marker_ident",
      angle="marker_angle"
    ) %>% 
    mutate(
      resp=toupper(resp),
      acc=marker_fame==resp
    ) %>% 
    mutate(
      fame=factor(marker_fame, levels=c("F", "N"), 
                  labels=c("famous", "novel"))
    ) %>% 
    mutate(
      ia=tolower(gsub(ia, pattern= " ", replacement = "")),
    ) %>% 
    mutate(
      subj = paste0(str_sub(toupper(subj), start=1, end=1), 
                    gsub("\\D", "", subj)),
      subj=case_when(
        # assign some controls to prosopagnosia group
        subj == "C17" ~ "D90",
        subj == "C26" ~ "D91",
        subj == "C35" ~ "D92",
        subj == "C38" ~ "D93",
        subj == "C44" ~ "D94",
        subj == "C62" ~ "D95",
        subj == "C70" ~ "D96",
        # assign some controls to super-recogniser group
        subj == "C13" ~ "S90",
        subj == "C68" ~ "S91",
        # make sure non-matching subjs get their existing value
        .default = subj
      )
    ) %>% 
    mutate(
      ability = factor(
        str_sub(subj,1,1),
        levels=c("C", "D", "S"),
        labels=c("control", "devprop", "super")
      )
    ) %>% 
    mutate(
      ia2 = factor(
        ia, 
        levels=c("hair", "forehead", "lefteye", "bridge", 
                 "righteye", "leftcheek", "nose", "rightcheek", 
                 "mouth", "chin"),
        labels=c("aB", "bB", "cA", "cB", "cC", "dA",
                 "dB", "dC", "eB", "fB"),
        exclude=NA
      )
    ) %>% 
    select(
      ability, subj, fame, face, angle, trial, acc, rt, time, ia, ia2
    ) %>% 
    arrange(
      ability, subj, fame, face, angle
    ) %>% 
    # assign bins
    group_by(subj,trial) %>%
    mutate(bin = 1 + (time %/% 50)) %>% 
    ungroup() %>% 
    # do modal IA
    group_by(subj, trial, bin) %>% 
    mutate(binia = names(which.max(table(ia2, useNA='always')))) %>% 
    ungroup() %>% 
    # remove these because they were properties of the row
    # and we are about to collapse to the bin
    select(-c(ia, ia2, time)) %>% 
    # collapse to row per bin
    distinct(subj, trial, bin, .keep_all = TRUE) %>% 
    # remove bins with modal ia of NA: 
    # if modal IA is NA then the most commonly fixated place in that bin 
    # was outside an interest area
    filter(!is.na(binia)) %>% 
    # collapse to row-per-trial by stringing the modal ias 
    # for that trial together
    group_by(subj, trial) %>% 
    mutate(iaseq = paste(binia, collapse="")) %>% 
    distinct(subj, trial, .keep_all = TRUE) %>% 
    select(-c(bin, binia))
  
  message("\tmerge data onto (full) design")
  # later we need a non-sparse design, so we should merge the real data onto 
  # the full design here.
  design = expand_grid(subj=unique(dat$subj), 
                       face=unique(dat$face), 
                       angle=unique(dat$angle)) %>% 
    mutate(
      ability=factor(str_sub(subj, 1, 1), 
                     levels=c("C", "D", "S"), 
                     labels=c("control", "devprop", "super")),
      fame=factor(str_sub(face, 1, 1), 
                  levels=c("F", "N"), labels=c("famous", "novel"))
    )
  # do merge, sorting on subj and trial
  dat <- 
    left_join(design, dat, 
              by = join_by(subj, ability, face, fame, angle)) %>% 
    arrange(subj, trial)
  # say what trial number the missing trials would have been
  nmissing = 0
  for(s in unique(dat$subj)){
    trials = subset(dat, subset=subj==s, select=trial) %>% pull(trial)
    if(length(na.omit(trials))<160){
      if(verbose){message("\nsubj: ", s)}
      missing_trials=setdiff(x=1:160, y=trials)
      for(k in 1:length(missing_trials)){
        nmissing = nmissing + 1
        if(verbose){message("\ttrial: ", missing_trials[k])}
      }
      dat[dat$subj==s & is.na(dat$trial),"trial"]=missing_trials
    }
  }
  if(verbose){message("nmissing was: ", nmissing)}
  # sort on subj and face and angle
  dat <- dat %>% arrange(subj, face, angle)
  
  message("\tnullify inaccurate trial scanpaths")
  # at the moment trials on which accuracy was FALSE have valid iaseq
  # We want to only enter accurate trials into the analysis,
  # but we don't want a sparse matrix so we replace those iaseq with "x"
  dat <- dat %>% mutate(iaseq = ifelse(acc==FALSE, "x", iaseq))
  
  # set types
  dat <- dat %>% 
    mutate(
      subj = as_factor(subj),
      face = as_factor(face),
      angle = as_factor(angle),
      trial = as.integer(trial),
      rt=as.integer(rt)
    )
  
  # add sequence number counter (set to zero)
  dat <- dat %>% 
    mutate(sq=as.numeric(0))
  
  # sort
  dat <- dat %>% 
    select(subj, ability, face, fame, angle, 
           trial, rt, acc, sq, everything()) %>% 
    arrange(subj, ability, face, fame, angle, trial)
  
  # 75 subjects
  #   ability
  #   control devprop   super 
  #   45      15        15 
  # 40 faces
  #   fame
  #   famous  novel 
  #   20      20 
  
  saveRDS(dat, binned_file)
  message("finished making binned sequences")
}