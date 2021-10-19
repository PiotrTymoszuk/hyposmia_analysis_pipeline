# This script calculates and compares times to recovery for particular symptoms -----

  insert_head()

# data container ----

  rec_time <- list()
  
# globals: symptom variables, analysis tables -----
  
  insert_msg('Globals setup')
  
  ## symptom variables
  
  rec_time$variables <- kinet$variables %>% 
    map(function(x) x[x != 'sympt_present'])
  
  ## analysis tables
  ## re-coding the time variable to account for the maximal symptom duration according to the questionnaire

  rec_time$analysis_tbl <- kinet$analysis_tbl
  
  rec_time$analysis_tbl$survey <- rec_time$analysis_tbl$survey %>% 
    map(mutate, 
        time = car::recode(time, "0 = 7; 7 = 14; 14 = 28; 28 = 90; 90 = 180"))
  
# serial analysis -----
  
  insert_msg('Calculating the times to recovery for particular individuals and symptoms')

  rec_time$sympt_duration <- c('covild', 'survey') %>% 
    map(function(x) rec_time$analysis_tbl[[x]] %>% 
          map(function(y) rec_time$variables[[x]] %>% 
                map(calculate_duration, 
                    data = y) %>% 
                reduce(left_join, by = 'ID'))) %>% 
    set_names(c('covild', 'survey'))

  
# Calculating median time to recovery -----
  
  insert_msg('Median time to recovery for particular symptoms')
  
  rec_time$median_duration <- rec_time$sympt_duration %>% 
    map(function(x) map(x, get_duration_stats, plot = F))
  
# visualization as a Forest plots, top factors with the most significant second order terms in kinetic modeling -----
  
  insert_msg('Plotting the duration distribution')
  
  ## CovILD
  
  rec_time$duration_plots$covild <- list(duration_tbl = rec_time$sympt_duration$covild %>% 
                                           map2(., kinet$top_second_order$covild, 
                                                ~select(.x, ID, any_of(.y))), 
                                         plot_title = paste0('CovILD: ', 
                                                             globals$sev_labels[names(rec_time$sympt_duration$covild)]), 
                                         plot_tag = paste('\nn =', map_chr(rec_time$sympt_duration$covild, nrow)), 
                                         color = globals$sev_colors[names(rec_time$sympt_duration$covild)]) %>% 
    pmap(get_duration_stats, 
         cust_geom = geom_blank, 
         dict = globals$var_lexicon, 
         plot = T, 
         median_txt = T)
  
  ## survey
  
  rec_time$duration_plots$survey <- list(duration_tbl = rec_time$sympt_duration$survey %>% 
                                           map2(., kinet$top_second_order$survey, 
                                                ~select(.x, ID, any_of(.y))), 
                                         plot_title = paste0('HACT: ', 
                                                             globals$cohort_labels[names(rec_time$sympt_duration$survey)]), 
                                         plot_tag = paste('\nn =', map_chr(rec_time$sympt_duration$survey, nrow)), 
                                         color = globals$cohort_colors[names(rec_time$sympt_duration$survey)]) %>% 
    pmap(get_duration_stats, 
         cust_geom = geom_blank, 
         dict = globals$survey_var_lexicon, 
         plot = T, 
         median_txt = T)
  
# END -----
  
  insert_tail()