# imports the pre-cleared Health after COVID (HACT, 
# observation time at least 28 days) and CovILD project data.
# Defines common globals, a shared format of the variable dictionaries 
# and calculates individual times to recovery for specific symptoms.
# The subset of the HACT study analyzed in the current project has an 
# observation time of at least 90 days and with acute symptoms.

  library(soucer)
  library(plyr)
  library(tidyverse)
  library(foreign)
  library(stringi)
  
  insert_head()
  
  source_all('./tools/project_tools.R', 
             message = TRUE, crash = TRUE)
  
# container lists ------
  
  rec_time <- list()
  mod_tbl <- list()
  
# globals ------
  
  insert_msg('Globals')
  
  source_all('./tools/project_globals.R', 
             message = TRUE, crash = TRUE)
  
# unpacking the data files -----
  
  insert_msg('Unpacking')
  
  load('./data/covild.RDa')
  load('./data/hact.RDa')
  
  hact[c('north', 'south')] <- hact[c('north', 'south')] %>% 
    map(filter, 
        obs_time >= 90, 
        acute_covid == 'yes') %>% 
    map(mutate, 
        comorb_present = ifelse(comorb_absent == 'yes', 'no', 'yes'), 
        comorb_present = factor(comorb_present, c('no', 'yes')), 
        incomplete_covelescence = ifelse(complete_covelescence == 'yes', 
                                         'no', 'yes'), 
        incomplete_covelescence = factor(incomplete_covelescence, c('no', 'yes')), 
        bmi_class_before = car::recode(bmi_class_before, 
                                       "'overweigth' = 'overweight'"), 
        bmi_class_before = factor(bmi_class_before, 
                                  c('normal', 'overweight', 'obesity')), 
        bmi_class_recent = car::recode(bmi_class_recent, 
                                       "'overweigth' = 'overweight'"), 
        bmi_class_recent = factor(bmi_class_recent, 
                                  c('normal', 'overweight', 'obesity')))

# Formatting the dictionaries -----
  
  insert_msg('Common format for the variable dictionaries')
  
  covild$dict <- covild$dict %>% 
    mutate(label_short = label)
  
  hact$dict  <- hact$dict %>% 
    select(variable, 
           label, 
           label_short, 
           unit, 
           description, 
           cutpoints, 
           levels) %>% 
    rbind(tibble(variable = 'incomplete_covelescence', 
                 label = 'Incomplete recovery', 
                 label_short = 'incomplete recovery', 
                 unit = NA, 
                 description = 'Self-perceived incomplete convalescence', 
                 cutpoints = NA, 
                 levels = 'no, yes')) %>% 
    rbind(tibble(variable = 'comorb_present', 
                 label = 'Comorbidity', 
                 label_short = 'comorbidity', 
                 unit = NA, 
                 description = 'At least one comorbidity present', 
                 cutpoints = NA, 
                 levels = 'no, yes')) %>% 
    mutate(axis_lab = ifelse(!is.na(unit), 
                             paste(label_short, unit, sep = ', '), 
                             label_short))
  
  hact$dict[hact$dict$variable == 'new_medication_fup', 'label_short'] <- 
    'new medication'
  
  hact$dict <- hact$dict %>% 
    mutate(label = stri_replace(label, 
                                fixed = 'Blue marmorated skin', 
                                replacement = 'Marmorated skin'), 
           label = stri_replace(label, 
                                fixed = 'Imp. fine motor skills', 
                                replacement = 'Imp. FMS'))
  
# Calculating the times to recovery, HACT -----
  
  insert_msg('Individual times to recovery, HACT')
  
  rec_time[c('north', 'south')] <- hact[c('north', 'south')] %>% 
    map(select, ID, all_of(globals$hact_symptoms)) %>% 
    map(~map_dfc(.x, 
                 car::recode, 
                 "'absent' = 0; 
                 '1 - 3 days' = 3; 
                 'up to 1 week' = 7; 
                 'up to 2 weeks' = 14; 
                 'up to 4 weeks' = 28; 
                 'up to 3 months' = 90; 
                 'up to 6 months' = 90; 
                 'over 6 months' = 90")) %>% 
    map(~map_dfc(.x, function(x) if(is.factor(x)) as.numeric(as.character(x)) else x))
  
# Calculating the times to recovery, CovILD ------
  
  insert_msg('Individual times to recovery, CovILD')
  
  ## maximal symptom duration, ignoring the remission/relapse
  
  rec_time$covild <- covild$data %>% 
    select(ID, time, all_of(globals$covild_symptoms)) %>% 
    dlply(.(time), as_tibble) %>% 
    map(select, - time) %>% 
    map2_dfr(., c(14, 60, 90, 180, 360), 
             ~map_dfc(.x, 
                      function(x) if(is.factor(x)) ifelse(x == 'yes', .y, 0) else x)) %>% 
    dlply(.(ID), as_tibble) %>% 
    map(~map_dfc(.x, 
                 function(x) if(is.numeric(x)) max(x, na.rm = TRUE) else x)) %>% 
    map_dfr(~.x[1, ]) %>% 
    left_join(covild$data %>% 
                filter(time == 4) %>% 
                select(ID, cat_WHO), 
              by = 'ID')
  
# kinetic modeling tables in long format, HACT -----
  
  insert_msg('Kinetic modeling table with the symptoms, HACT')
  
  ## presence of the particular symptom at the 3, 7, 14 and so on
  ## time points
  
  mod_tbl[c('north', 'south')] <- hact[c('north', 'south')] %>% 
    map(select, ID, all_of(globals$hact_symptoms))
  
  mod_tbl[c('north', 'south')] <- mod_tbl[c('north', 'south')] %>% 
    map(function(cohort) cohort %>% 
          dlply(.(ID), as_tibble) %>% 
          map(select, -ID) %>% 
          map(~map_dfc(.x, function(sympt) switch(as.character(sympt), 
                                                  absent = c(0, 0, 0, 0, 0), 
                                                  `1 - 3 days` = c(1, 0, 0, 0, 0), 
                                                  `up to 1 week` = c(1, 1, 0, 0, 0), 
                                                  `up to 2 weeks` = c(1, 1, 1, 0, 0), 
                                                  `up to 4 weeks` = c(1, 1, 1, 1, 0), 
                                                  `up to 3 months` = c(1, 1, 1, 1, 1), 
                                                  `up to 6 months` = c(1, 1, 1, 1, 1), 
                                                  `over 6 months` = c(1, 1, 1, 1, 1)))) %>% 
          map2_dfr(., names(.), ~mutate(.x, ID = .y, time = c(3, 7, 14, 28, 90))))
  
# kinetic modeling table, COVILD ------
  
  insert_msg('Kinetic modeling table, CovILD')
  
  mod_tbl$covild <- covild$data %>% 
    select(ID, cat_WHO, time_numeric, all_of(globals$covild_symptoms))
  
# reading the sniffing stick test results -----
  
  insert_msg('Sniffing stick test results')
  
  sst <- list()
  
  sst$data_100_fup <- read.spss('./data/COVIDPat_135_cluster.sav', 
                                to.data.frame = TRUE) %>% 
    transmute(ID = stri_replace(pat_id, regex = '\\s+$', replacement = ''), 
              sniff_score = as.numeric(sniffin_sticks_score), 
              sniff_score_cut = cut(sniff_score, 
                                    c(-Inf, 8, 12, Inf), 
                                    c('severe', 'moderate', 'normal')), 
              sniff_score_cut = factor(sniff_score_cut, 
                                       c('normal', 'moderate', 'severe')), 
              sniff_hyposmia = ifelse(sniff_score < 13, 'yes', 'no'), 
              sniff_hyposmia = factor(sniff_hyposmia, c('no', 'yes'))) %>% 
    left_join(covild$data %>% 
                filter(time == 1) %>% 
                select(ID, cat_WHO, anosmia_sympt), 
              ., 
              by = 'ID') %>% 
    filter(complete.cases(.))
  
  sst$data_360_fup <- read.spss('./data/ATTRACT_81pts_hyposmia.sav', 
                                to.data.frame = TRUE) %>% 
    transmute(ID = stri_replace(PatID, regex = '\\s+$', replacement = ''), 
              sniff_score = as.numeric(sniff_score_1a), 
              sniff_score_cut = cut(sniff_score, 
                                    c(-Inf, 8, 12, Inf), 
                                    c('severe', 'moderate', 'normal')), 
              sniff_score_cut = factor(sniff_score_cut, 
                                       c('normal', 'moderate', 'severe')), 
              sniff_hyposmia = ifelse(sniff_score < 13, 'yes', 'no'), 
              sniff_hyposmia = factor(sniff_hyposmia, c('no', 'yes'))) %>% 
    left_join(covild$data %>% 
                filter(time == 4) %>% 
                select(ID, cat_WHO, anosmia_sympt), 
              ., 
              by = 'ID') %>% 
    filter(complete.cases(.))
  
# manual editing of the variable dictionary -------
  
  insert_msg('Manual editing of the variable dictionary')
  
  hact$dict <- hact$dict %>% 
    mutate(label = ifelse(variable == 'anosmia', 
                          'OD', label), 
           label_short = ifelse(variable == 'anosmia', 
                                'OD', label_short), 
           label = ifelse(variable == 'taste_loss', 
                          'Hypogeusia/ageusia', label), 
           label_short = ifelse(variable == 'taste_loss', 
                                'hypogeusia/ageusia', label_short))
  
  covild$dict <- covild$dict %>% 
    mutate(label = ifelse(variable == 'anosmia_sympt', 
                          'OD', label), 
           label_short = ifelse(variable == 'anosmia_sympt', 
                                'OD', label))
  
# END ----
  
  insert_tail()