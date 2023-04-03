# Frequency of acute, sub-acute and long and PACS symptoms.

  insert_head()
  
# container list -----
  
  eda_sympt <- list()
  
# Globals ------
  
  insert_msg('Globals setup')
  
  ## analysis tables, HACT
  
  eda_sympt$hact_tbl <- mod_tbl[c('north', 'south')] %>% 
    map2_dfr(., names(.), ~mutate(.x, cohort = .y)) %>% 
    filter(time %in% c(3, 14, 28, 90)) %>% 
    dlply('time', select, -ID, -time) %>% 
    set_names(c('acute', 'sub_acute', 'long', 'pasc')) %>% 
    map(as_tibble) %>% 
    map(map_dfr, factor)

  ## analysis tables, covILD
  
  eda_sympt$covild_tbl <- mod_tbl$covild %>% 
    complete_cases %>% 
    dlply('time_numeric', as_tibble)
  
  ## n numbers
  
  eda_sympt$hact_n_tags <- paste('\nn =', 
                                 dlply(eda_sympt$hact_tbl$acute, 
                                       'cohort', 
                                       nrow) %>% 
                                   unlist)
  
  eda_sympt$covild_n_tags <- paste('\nn =', 
                                   dlply(eda_sympt$covild_tbl[[1]], 
                                         'cat_WHO', 
                                         nrow) %>% 
                                     unlist)
  
  ## parallel backend
  
  plan('multisession')
  
# descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  eda_sympt$hact_stats <- eda_sympt$hact_tbl %>% 
    future_map(explore, 
               split_factor = 'cohort', 
               variables = globals$hact_symptoms, 
               what = 'table', 
               pub_styled = TRUE, 
               .options = furrr_options(seed = TRUE)) %>% 
    future_map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', 'AT', 'IT')) %>% 
    map(map_dfr, stri_replace_all, regex = '0:.*\\n1:\\s{1}', replacement = '') %>% 
    map(map_dfr, stri_replace_all, fixed = '% (', replacement = '% (n = ') %>% 
    map(map_dfr, stri_replace, fixed = 'Complete', replacement = 'complete')
  
  eda_sympt$covild_stats <- eda_sympt$covild_tbl %>% 
    map(explore, 
        split_factor = 'cat_WHO', 
        variables = globals$covild_symptoms, 
        what = 'table', 
        pub_styled = TRUE) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', 'A', 'HM', 'HS')) %>% 
    map(map_dfr, stri_replace_all, regex = 'no:.*\\nyes:\\s{1}', replacement = '') %>% 
    map(map_dfr, stri_replace_all, fixed = '% (', replacement = '% (n = ') %>% 
    map(map_dfr, stri_replace, fixed = 'Complete', replacement = 'complete')
  
# testing for the differences in frequency between the cohorts or severity -----
  
  insert_msg('Testing')
  
  eda_sympt$hact_test <- eda_sympt$hact_tbl %>% 
    future_map(compare_variables, 
               split_factor = 'cohort', 
               variables = globals$hact_symptoms, 
               what = 'test',
               types = 'chisq_test', 
               ci = FALSE, 
               adj_method = 'BH', 
               pub_styled = TRUE, 
               .options = furrr_options(seed = TRUE))
  
  eda_sympt$covild_test <- eda_sympt$covild_tbl %>% 
    map(compare_variables, 
        split_factor = 'cat_WHO', 
        variables = globals$covild_symptoms, 
        what = 'test',
        types = 'chisq_test', 
        ci = FALSE, 
        adj_method = 'BH', 
        pub_styled = TRUE)
  
# Common table with the stats and testing results -----
  
  insert_msg('Common table with the stats and testing results')
  
  eda_sympt$hact_cmm_tbl <- map2(eda_sympt$hact_stats, 
                                 map(eda_sympt$hact_test, 
                                     select, variable, significance, eff_size), 
                                 left_join, by = 'variable')
  
  eda_sympt$covild_cmm_tbl <- map2(eda_sympt$covild_stats, 
                                   map(eda_sympt$covild_test, 
                                       select, variable, significance, eff_size), 
                                   left_join, by = 'variable')
  
# Summary tables with the symptom frequencies ------
  
  insert_msg('Summary of the symptom frequencies')
  
  eda_sympt$hact_sympt_freq <- eda_sympt$hact_tbl %>% 
    future_map(explore, 
               split_factor = 'cohort', 
               variables = globals$hact_symptoms, 
               what = 'list', 
               .options = furrr_options(seed = TRUE)) %>% 
    map(~map(.x, ~map2_dfr(.x, names(.x), ~mutate(.x$statistic, variable = .y)))) %>% 
    map(~map(.x, filter, category == 1)) %>% 
    map(~map(.x, select, - category)) %>% 
    map(~map(.x, arrange, -percent))
  
  eda_sympt$covild_sympt_freq <- eda_sympt$covild_tbl %>% 
    map(explore, 
        split_factor = 'cat_WHO', 
        variables = globals$covild_symptoms, 
        what = 'list') %>% 
    map(~map(.x, ~map2_dfr(.x, names(.x), ~mutate(.x$statistic, variable = .y)))) %>% 
    map(~map(.x, filter, category == 'yes')) %>% 
    map(~map(.x, select, - category)) %>% 
    map(~map(.x, arrange, -percent))
  
# Top 10 most frequnet symptoms in the HACT study for each time point -----
  
  insert_msg('Top 10 symptoms')
  
  eda_sympt$hact_top <- eda_sympt$hact_sympt_freq %>% 
    map(~map(.x, ~.x$variable[1:10])) %>% 
    map(reduce, intersect)
  
# Summary bubble plots with the symptom frequencies, HACT -----
  
  insert_msg('Bubble with the symptom frequency in the HACT study')
  
  eda_sympt$hact_bubble <- eda_sympt$hact_sympt_freq %>% 
    transpose %>% 
    map(~.x[c('acute', 'long', 'pasc')]) %>% 
    map(~map2_dfr(.x, names(.x), ~mutate(.x, timepoint = .y))) %>% 
    map(left_join, globals$hact_sympt_class, by = 'variable')
  
  eda_sympt$hact_bubble <- list(data = eda_sympt$hact_bubble, 
                                plot_subtitle = paste(c('AT, survey study', 
                                                        'IT, survey study'), 
                                                      '% cohort', sep = ', '), 
                                plot_tag = eda_sympt$hact_n_tags) %>% 
    pmap(draw_freq_bubble, 
         plot_title = 'Symptom frequency', 
         x_lab = '') %>% 
    map(~.x + 
          scale_y_discrete(labels = translate_var(globals$hact_symptoms, 
                                                  dict = hact$dict)) + 
          scale_size_area(max_size = 4) + 
          scale_x_discrete(labels = c(acute = '0 - 14 days', 
                                      long = '28 days', 
                                      pasc = '3 months')) + 
          scale_fill_viridis_c(limits = c(0, 100)) + 
          #scale_fill_gradient2(low = 'steelblue', 
            #                   mid = 'white', 
             #                  high = 'firebrick', 
              #                 midpoint = 50, 
               #                limits = c(0, 100)) + 
          facet_grid(class ~ ., 
                     scales = 'free', 
                     space = 'free') + 
          theme(strip.background = element_blank(), 
                strip.text = element_blank()) + 
          guides(fill = FALSE, 
                 size = FALSE))
  
# summary bubble plots with the frequencies of symptoms in the CovILD study -----
  
  insert_msg('Bubble plots with the symptom frequencies, CovILD')
  
  eda_sympt$covild_bubble <- eda_sympt$covild_sympt_freq %>% 
    transpose %>% 
    map(~map2_dfr(.x, names(.x), ~mutate(.x, timepoint = .y))) %>% 
    map(mutate, timepoint = factor(timepoint, c('0', '60', '100', '180', '360')))
  
  eda_sympt$covild_bubble <- list(data = eda_sympt$covild_bubble, 
                                  plot_subtitle =  paste(c('Ambulatory CoV', 
                                                           'Moderate CoV', 
                                                           'Severe CoV'), 
                                                         'CovILD study, % severity strata', 
                                                         sep = ', '), 
                                  plot_tag = eda_sympt$covild_n_tags) %>% 
    pmap(draw_freq_bubble, 
         plot_title = 'Symptom frequency', 
         x_lab = 'Days post CoV', 
         txt_hjust = -0.6) %>% 
    map(~.x + 
          scale_y_discrete(labels = translate_var(globals$covild_symptoms, 
                                                  dict = covild$dict), 
                           limits = rev(c('fatigue_sympt', 
                                          'sleep_sympt', 
                                          'night_sweat_sympt', 
                                          'anosmia_sympt', 
                                          'dyspnoe_sympt', 
                                          'cough_sympt', 
                                          'gastro_sympt', 
                                          'fever_sympt'))) + 
          scale_size_area(max_size = 4) + 
          scale_fill_viridis_c(limits = c(0, 100)) + 
         # scale_fill_gradient2(low = 'steelblue', 
           #                    mid = 'white', 
            #                   high = 'firebrick', 
            #                   midpoint = 50, 
             #                  limits = c(0, 100)) + 
          guides(fill = FALSE, 
                 size = FALSE))
  
# END ----
  
  plan('sequential')
  
  insert_tail()