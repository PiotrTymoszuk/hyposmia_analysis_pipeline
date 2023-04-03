# Snbiffing stick versus self-reported hyposmia

  insert_head()
  
# container list -----
  
  rater <- list()
  
# globals: analysis tables ----
  
  insert_msg('Globals setup')
  
  rater$analysis_tbl <- list(fup100 = sst$data_100_fup, 
                             fup360 = sst$data_360_fup) %>% 
    map(~dlply(.x, 'cat_WHO', as_tibble)) %>% 
    map2(., sst[c('data_100_fup', 'data_360_fup')], 
         ~c(.x, list(cohort = .y))) %>% 
    unlist(recursive = FALSE)
  
# calculation of the kappas -----
  
  insert_msg('Calculation of Kohen kappas')
  
  rater$test_results <- rater$analysis_tbl %>% 
    map(~correlate_variables(.x, 
                             variables = c('anosmia_sympt', 'sniff_hyposmia'), 
                             what = 'correlation', 
                             type = 'kappa', 
                             ci = TRUE, 
                             pub_styled = TRUE)) %>% 
    map2_dfr(., names(.), ~mutate(.x, 
                                  time = stri_split_fixed(.y, pattern = '.', simplify = TRUE)[, 1], 
                                  subset = stri_split_fixed(.y, pattern = '.', simplify = TRUE)[, 2])) %>% 
    re_adjust %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))

# Plotting the interrater results -------
  
  insert_msg('Plotting the interrater results, bar plots')
  
  rater$bar_plots <- list(data = rater$analysis_tbl, 
                          plot_title = rep(c('Ambulatory, CovILD study', 
                                             'Moderate, CovILD study', 
                                             'Severe, CovILD study', 
                                             'CovILD cohort'), 2), 
                          plot_subtitle = rater$test_results$plot_cap) %>% 
    pmap(plot_correlation, 
         variables = c('anosmia_sympt', 'sniff_hyposmia'), 
         scale = 'percent', 
         type = 'bar', 
         x_lab = 'OD', 
         cust_theme = globals$common_theme) %>% 
    map2(.,map(rater$analysis_tbl, nrow), 
         ~.x + 
           labs(subtitle = paste0(.x$labels$subtitle, ', n = ', .y)) + 
           scale_fill_manual(values = c('bisque3', 'steelblue'), 
                             labels = c('self-reported', 'test'), 
                             name = 'OD'))
  
# Plotting the interrater results, bubble -----
  
  insert_msg('Plotting the interrater results, bubble plots')
  
  rater$bubble_plots <- list(data = rater$analysis_tbl, 
                             plot_title = rep(c('Ambulatory, CovILD study', 
                                                'Moderate, CovILD study', 
                                                'Severe, CovILD study', 
                                                'CovILD cohort'), 2), 
                             plot_subtitle = rater$test_results$plot_cap) %>% 
    pmap(plot_correlation, 
         variables = c('anosmia_sympt', 'sniff_hyposmia'), 
         scale = 'percent', 
         type = 'bubble', 
         x_lab = 'OD', 
         point_alpha = 1, 
         cust_theme = globals$common_theme) %>% 
    map2(.,map(rater$analysis_tbl, nrow), 
         ~.x + 
           labs(subtitle = paste0(.x$labels$subtitle, ', n = ', .y), 
                x = 'Self-reported OD', 
                y = 'Test OD') + 
           scale_size(limits = c(0, 71)) + 
           scale_fill_gradient2(low = 'steelblue', 
                                mid = 'white', 
                                high = 'firebrick', 
                                midpoint = 30, 
                                limits = c(0, 71)) +
           guides(fill = FALSE, 
                  size = FALSE) + 
           theme(plot.tag = element_blank()))
  
# END -----
  
  insert_tail()