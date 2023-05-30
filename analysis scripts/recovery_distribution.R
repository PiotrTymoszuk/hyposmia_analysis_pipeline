# Analyzes and visualizes the time to recovery for particular symptoms

  insert_head()

# container list ------

  rec_dist <- list()
  
# Parallel backend -------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# globals -------
  
  insert_msg('Globals setup')
  
  rec_dist$tbl_hact <- rec_time[c('north', 'south')] %>% 
    compress(names_to = 'cohort') %>% 
    mutate(cohort = car::recode(cohort, "'north' = 'AT'; 'south' = 'IT'"), 
           cohort = factor(cohort, c('AT', 'IT')))
  
# descriptive stats of the recovery: individuals with the symptom present -----  
  
  insert_msg('Distribution stats for the rec. times, symptomatic  participants')
  
  rec_dist$stats_hact <- globals$hact_symptoms %>% 
    future_map_dfr(~explore(rec_dist$tbl_hact[c('cohort', .x)] %>% 
                              filter(.data[[.x]] != 0), 
                            split_factor = 'cohort', 
                            variables = .x, 
                            what = 'table', 
                            pub_styled = TRUE), 
                   .options = furrr_options(seed = TRUE)) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(rec_dist$tbl_hact$cohort)))
  
  rec_dist$stats_covild <- globals$covild_symptoms %>% 
    map_dfr(~explore(rec_time$covild[c('cat_WHO', .x)] %>% 
                       filter(.data[[.x]] != 0), 
                     split_factor = 'cat_WHO', 
                     variables = .x, 
                     what = 'table', 
                     pub_styled = TRUE)) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(rec_time$covild$cat_WHO)))

# testing, differences between the cohorts or severity strata, symptomatic ----

  insert_msg('Testing for the differences between the cohorts or severity')
  
  rec_dist$test_hact <- globals$hact_symptoms %>% 
    future_map(~safely(compare_variables)(rec_dist$tbl_hact[c('cohort', .x)] %>% 
                                            filter(.data[[.x]] != 0), 
                                          split_factor = 'cohort', 
                                          variables = .x, 
                                          what = 'eff_size', 
                                          types = 'wilcoxon_r', 
                                          ci = FALSE, 
                                          pub_styled = TRUE, 
                                          adj_method = 'none'), 
               .options = furrr_options(seed = TRUE))

  rec_dist$test_covild <- globals$covild_symptoms %>% 
    map(~safely(compare_variables)(rec_time$covild[c('cat_WHO', .x)] %>% 
                                     filter(.data[[.x]] != 0), 
                                   split_factor = 'cat_WHO', 
                                   variables = .x, 
                                   what = 'test', 
                                   types = 'kruskal_test', 
                                   ci = FALSE, 
                                   pub_styled = TRUE, 
                                   adj_method = 'none'))

  rec_dist[c('test_hact', 
             'test_covild')] <- rec_dist[c('test_hact', 
                                           'test_covild')] %>% 
    map(~map_dfr(.x, ~.x$result)) %>% 
    map(re_adjust) %>% 
    map(mutate, plot_cap = paste(eff_size, significance, sep = ', '))
  
# Violin plots with the recovery times for the particular symptoms ------

  insert_msg('Violin plots, single symptoms')
  
  ## HACT
  
  rec_dist$violin_hact <- list(x = rec_dist$test_hact$variable, 
                               y = exchange(rec_dist$test_hact$variable, 
                                            dict = hact$dict), 
                               z = rec_dist$test_hact$plot_cap) %>% 
    pmap(function(x, y, z) plot_variable(rec_dist$tbl_hact[c('cohort', x)] %>% 
                                           filter(.data[[x]] != 0), 
                                         split_factor = 'cohort', 
                                         variable = x, 
                                         type = 'stack', 
                                         scale = 'percent', 
                                         point_alpha = 0.3, 
                                         point_hjitter = 0.4, 
                                         cust_theme = globals$common_theme, 
                                         plot_title = y, 
                                         plot_subtitle = z, 
                                         x_n_labs = TRUE)) %>% 
    map(~.x + 
           scale_fill_brewer(palette = 'Reds', 
                             name = 'Median recovery, days') + 
           theme(axis.title.x = element_blank())) %>% 
    set_names(rec_dist$test_hact$variable)
  
  ## CovILD
  
  rec_dist$violin_covild <- list(x = rec_dist$test_covild$variable, 
                                 y = exchange(rec_dist$test_covild$variable, 
                                              dict = covild$dict), 
                                 z = rec_dist$test_covild$plot_cap) %>% 
    pmap(function(x, y, z) plot_variable(rec_time$covild[c('cat_WHO', x)] %>% 
                                           filter(.data[[x]] != 0), 
                                         split_factor = 'cat_WHO', 
                                         variable = x, 
                                         type = 'stack', 
                                         scale = 'percent', 
                                         point_alpha = 0.5, 
                                         point_hjitter = 0.4, 
                                         cust_theme = globals$common_theme, 
                                         plot_title = y, 
                                         plot_subtitle = z,
                                         x_n_labs = TRUE)) %>% 
    map(~.x + 
          scale_fill_brewer(palette = 'Blues', 
                            name = 'Median recovery, days') + 
          theme(axis.title.x = element_blank())) %>% 
    set_names(rec_dist$test_covild$variable)
  
# A summary visualization of the median recovery times -------
  
  insert_msg('Summary plot')
  
  ## HACT

  rec_dist$summ_plot_hact <- list(data = rec_time[c('north', 'south')] %>% 
                                    map(select, - ID) %>% 
                                    map(~set_names(.x, 
                                                   exchange(names(.x), 
                                                            dict = hact$dict))), 
                                  plot_subtitle = map2(c('AT, survey study', 
                                                         'IT, survey study'), 
                                                       rec_time[c('north', 
                                                                  'south')], 
                                                       ~paste(.x, nrow(.y), 
                                                              sep = ', n = ')), 
                                  median_color = globals$hact_colors, 
                                  fill = globals$hact_colors) %>% 
    pmap(draw_quantile_elli, 
         variables = exchange(globals$hact_symptoms, dict = hact$dict), 
         ell_width = 0.3, 
         non_zero = TRUE, 
         alpha = 0.3, 
         plot_title = 'Symptom recovery time', 
         x_lab = 'Recovery time, days post CoV\nmedian with IQR', 
         cust_theme = globals$common_theme)
  
  ## covild
  
  rec_dist$summ_plot_covild <- list(data = rec_time$covild %>% 
                                      dlply('cat_WHO', select, -ID, -cat_WHO) %>% 
                                      map(~set_names(.x, 
                                                     exchange(names(.x), 
                                                              dict = covild$dict))), 
                                    plot_subtitle = paste0(c('Ambulatory CoV', 
                                                             'Moderate CoV', 
                                                             'Severe CoV'), 
                                                           ', CovILD study'), 
                                    median_color = globals$covild_colors[1:3], 
                                    fill = globals$covild_colors[1:3]) %>% 
    pmap(draw_quantile_elli, 
         variables = exchange(globals$covild_symptoms, dict = covild$dict), 
         ell_width = 0.3, 
         non_zero = TRUE, 
         alpha = 0.3, 
         plot_title = 'Symptom recovery time', 
         x = 'Recovery time, days post CoV\nmedian with IQR', 
         cust_theme = globals$common_theme)

  
# END -----
  
  insert_tail()