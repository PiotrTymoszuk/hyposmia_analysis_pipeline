# Frequency of acute, sub-acute and long and PASC symptoms.

  insert_head()
  
# container list -----
  
  eda_sympt <- list()
  
# Globals ------
  
  insert_msg('Globals setup')
  
  ## analysis tables, HACT
  
  eda_sympt$hact_tbl <- mod_tbl[c('north', 'south')] %>% 
    compress(names_to = 'cohort') %>% 
    filter(time %in% c(3, 14, 28, 90)) %>% 
    blast(time) %>% 
    map(select, -ID, -time) %>% 
    set_names(c('acute', 'sub_acute', 'long', 'pasc')) %>% 
    map(as_tibble) %>% 
    map(map_dfr, car::recode, "0 = 'no'; 1 = 'yes'") %>% 
    map(map_dfr, factor)

  ## analysis tables, covILD
  
  eda_sympt$covild_tbl <- mod_tbl$covild %>% 
    complete_cases %>% 
    blast(time_numeric)
  
  ## n numbers
  
  eda_sympt$hact_n_numbers <- eda_sympt$hact_tbl$acute %>% 
    blast(cohort) %>% 
    map_dbl(nrow)
  
  eda_sympt$hact_n_tags <- eda_sympt$hact_n_numbers %>% 
    paste('n = ', .)
    
  eda_sympt$covild_n_numbers <- eda_sympt$covild_tbl[[1]] %>% 
    blast(cat_WHO) %>% 
    map_dbl(nrow)
  
  eda_sympt$covild_n_tags <-  eda_sympt$covild_n_numbers %>% 
    paste('n = ', .)
  
  ## labels for the HACT time points
  
  eda_sympt$hact_time_labs <- 
    c(acute = '0 - 14 days', 
      long = '28 days', 
      pasc = '3 months')

  ## parallel backend
  
  plan('multisession')
  
# descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  ## HACT
  
  eda_sympt$hact_stats <- eda_sympt$hact_tbl %>% 
    future_map(explore, 
               split_factor = 'cohort', 
               variables = globals$hact_symptoms, 
               what = 'table', 
               pub_styled = TRUE, 
               .options = furrr_options(seed = TRUE)) %>% 
    future_map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', 'AT', 'IT'))
  
  ## CovILD

  eda_sympt$covild_stats <- eda_sympt$covild_tbl %>% 
    map(explore, 
        split_factor = 'cat_WHO', 
        variables = globals$covild_symptoms, 
        what = 'table', 
        pub_styled = TRUE) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', 'A', 'HM', 'HS'))
  
  ## formatting
  
  for(i in c("hact_stats", "covild_stats")) {
    
    eda_sympt[[i]] <- eda_sympt[[i]] %>% 
      map(~map_dfc(.x, 
                   stri_replace, 
                   regex = 'no:.*\\nyes:\\s{1}', 
                   replacement = '') %>% 
            map_dfc(stri_replace_all, 
                    fixed = '% (', 
                    replacement = '% (n = ') %>% 
            map_dfc(stri_replace,
                    regex = '\\nComplete.*$', 
                    replacement = ''))
    
  }
  
  ## appending with the N numbers
  
  eda_sympt$hact_stats <- eda_sympt$hact_stats %>% 
    map(mutate, 
        variable = exchange(variable, 
                            dict = hact$dict)) %>% 
    map(~rbind(tibble(variable = 'Participants, n', 
                      AT = eda_sympt$hact_n_numbers['north'], 
                      IT = eda_sympt$hact_n_numbers['south']), 
               .x))
  
  eda_sympt$covild_stats <- eda_sympt$covild_stats %>% 
    map(mutate, 
        variable = exchange(variable, 
                            dict = covild$dict)) %>% 
    map(~rbind(tibble(variable = 'Participants, n', 
                      A = eda_sympt$covild_n_numbers['A'], 
                      HM = eda_sympt$covild_n_numbers['HM'], 
                      HS = eda_sympt$covild_n_numbers['HS']), 
               .x))

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
  
  eda_sympt$hact_cmm_tbl <- 
    map2(eda_sympt$hact_stats, 
         map(eda_sympt$hact_test, 
             select, variable, significance, eff_size), 
         left_join, by = 'variable')
  
  eda_sympt$covild_cmm_tbl <- 
    map2(eda_sympt$covild_stats, 
         map(eda_sympt$covild_test, 
             select, variable, significance, eff_size), 
         left_join, by = 'variable')
  
# Summary tables with the symptom frequencies ------
  
  insert_msg('Summary of the symptom frequencies')
  
  ## HACT
  
  eda_sympt$hact_sympt_freq <- eda_sympt$hact_tbl %>% 
    future_map(explore, 
               split_factor = 'cohort', 
               variables = globals$hact_symptoms, 
               what = 'list', 
               .options = furrr_options(seed = TRUE))
  
  ## CovILD
  
  eda_sympt$covild_sympt_freq <- eda_sympt$covild_tbl %>% 
    map(explore, 
        split_factor = 'cat_WHO', 
        variables = globals$covild_symptoms, 
        what = 'list')
  
  ## formatting 
  
  for(i in c('hact_sympt_freq', 'covild_sympt_freq')) {
    
    eda_sympt[[i]] <- eda_sympt[[i]] %>% 
      map(~map(.x, 
               ~map2_dfr(.x, names(.x), 
                         ~mutate(.x$statistic, variable = .y)))) %>% 
      map(~map(.x, filter, category == 'yes')) %>% 
      map(~map(.x, select, - category)) %>% 
      map(~map(.x, arrange, -percent))
    
  }
  
# Top 10 most frequent symptoms in the HACT study for each time point -----
  
  insert_msg('Top 10 symptoms')
  
  eda_sympt$hact_top <- eda_sympt$hact_sympt_freq %>% 
    map(~map(.x, ~.x$variable[1:10])) %>% 
    map(reduce, intersect)
  
# Summary bubble plots with the symptom frequencies, HACT -----
  
  insert_msg('Bubble with the symptom frequency in the HACT study')
  
  eda_sympt$hact_bubble <- eda_sympt$hact_sympt_freq %>% 
    transpose %>% 
    map(~.x[c('acute', 'long', 'pasc')]) %>% 
    map(compress, names_to = 'timepoint') %>% 
    map(left_join, 
        globals$hact_sympt_class, 
        by = 'variable')
  
  eda_sympt$hact_bubble <- 
    list(data = eda_sympt$hact_bubble, 
         plot_subtitle = map2(c('AT, survey study', 
                                'IT, survey study'), 
                              eda_sympt$hact_n_tags, 
                              paste, sep = ', % of cohort, ')) %>% 
    pmap(draw_freq_bubble, 
         plot_title = 'Symptom frequency', 
         x_lab = '') %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) exchange(x, dict = hact$dict)) + 
          scale_size_area(max_size = 4) + 
          scale_x_discrete(labels = eda_sympt$hact_time_labs) + 
          scale_fill_viridis_c(limits = c(0, 100)) + 
          facet_grid(class ~ ., 
                     scales = 'free', 
                     space = 'free') + 
          theme(strip.background = element_blank(), 
                strip.text = element_blank()) + 
          guides(fill = 'none', 
                 size = 'none'))
  
# Summary bubble plots with the frequencies of symptoms in the CovILD study -----
  
  insert_msg('Bubble plots with the symptom frequencies, CovILD')
  
  eda_sympt$covild_bubble <- eda_sympt$covild_sympt_freq %>% 
    transpose %>% 
    map(compress, names_to = 'timepoint') %>% 
    map(mutate, 
        timepoint = factor(timepoint, c('0', '60', '100', '180', '360')))
  
  eda_sympt$covild_bubble <- 
    list(data = eda_sympt$covild_bubble, 
         plot_subtitle = map2(c('Ambulatory CoV', 
                                'Moderate CoV', 
                                'Severe CoV'), 
                              eda_sympt$covild_n_tags, 
                              paste, 
                              sep = ', CovILD study, % of severity strata, ')) %>% 
    pmap(draw_freq_bubble, 
         plot_title = 'Symptom frequency', 
         x_lab = 'Days post CoV', 
         txt_hjust = -0.6) %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) exchange(x, dict = covild$dict), 
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
          guides(fill = FALSE, 
                 size = FALSE))
  
# Plotting significant differences in symptom frequency, HACT ------
  
  insert_msg('Plots of significant differences in symptoms, HACT')
  
  ## symptoms of interest
  
  eda_sympt$signif_hact$symptoms <- 
    eda_sympt$hact_test[c('acute', 'long', 'pasc')] %>% 
    map(filter, p_adjusted < 0.05) %>% 
    map(~.x$variable) %>% 
    reduce(union)
  
  ## percentages of the symptoms of interest per timepoints
  ## appending with the testing results
  
  eda_sympt$signif_hact$data <- 
    eda_sympt$hact_tbl[c('acute', 'long', 'pasc')] %>% 
    map(select, 
        cohort, 
        all_of(eda_sympt$signif_hact$symptoms)) %>% 
    map(blast, cohort) %>% 
    map(map, select, -cohort) %>% 
    map(map, map_dfc, as_numeric) %>% 
    map(map, map_dfc, function(x) x - 1) %>% 
    map(~map(.x, ~colSums(.x)/nrow(.x) * 100)) %>% 
    map(map, compress, 
        names_to = 'variable', 
        values_to = 'percent') %>% 
    map(compress, names_to = 'cohort')
  
  eda_sympt$signif_hact$data <- 
    map2(eda_sympt$signif_hact$data, 
         eda_sympt$hact_test[c('acute', 'long', 'pasc')] %>% 
           map(~.x[c('variable', 'eff_size', 'significance')]), 
         left_join, by = 'variable') %>% 
    map(mutate, 
        axis_lab = exchange(variable, dict = hact$dict), 
        axis_lab = paste(axis_lab, 
                         paste(eff_size, significance, sep = ', '), 
                         sep = '\n'))
  
  ## n numbers per cohort
  
  eda_sympt$signif_hact$n_labs <- 
    map2_chr(globals$hact_labs, 
             eda_sympt$hact_n_tags, 
             paste, sep = ': ') %>% 
    paste(collapse = ', ')
  
  ## plots
  
  eda_sympt$signif_hact$plots <- 
    list(x = eda_sympt$signif_hact$data %>% 
           map(filter, !stri_detect(significance, regex = '^ns')), 
         y = paste('Survey study,', 
                   eda_sympt$hact_time_labs), 
         z = c(80, 20, 15)) %>% 
    pmap(function(x, y, z) x %>% 
           ggplot(aes(x = percent, 
                      y = reorder(axis_lab, percent), 
                      fill = cohort)) + 
           geom_bar(stat = 'identity', 
                    color = 'black', 
                    position = position_dodge(0.9)) + 
           geom_text(aes(label = paste0(signif(percent, 2), '%'),
                         color = cohort), 
                     size = 2.75, 
                     hjust = -0.4, 
                     position = position_dodge(0.9)) + 
           scale_fill_manual(values = globals$hact_colors, 
                             labels = globals$hact_labs, 
                             name = '') + 
           scale_color_manual(values = globals$hact_colors, 
                              labels = globals$hact_labs, 
                              name = '') + 
           scale_x_continuous(limits = c(0, z)) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = y, 
                subtitle = eda_sympt$signif_hact$n_labs,
                x = '% of cohort'))

# END ----
  
  plan('sequential')
  
  rm(i)
  
  insert_tail()