# Demographic, cinical and recovery characteristic of the participant clusters

  insert_head()
  
# container list ------
  
  clust_chara <- list()
  
# parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# globals ------
  
  insert_msg('Globals setup')
  
  ## types of the tests and plots
  
  clust_chara$var_types <- globals[c('demo_vars', 
                                     'clinic_vars', 
                                     'cov_vars', 
                                     'psych_var')] %>% 
    reduce(c) %>% 
    map_dfr(~tibble(variable = .x, 
                    type_test = ifelse(is.numeric(hact$north[[.x]]), 
                                       'kruskal_test', 'chisq_test'), 
                    type_plot = ifelse(is.numeric(hact$north[[.x]]), 
                                       'violin', 'stack'), 
                    y_lab = ifelse(is.numeric(hact$north[[.x]]), 
                                   exchange(.x, 
                                            value = 'axis_lab', 
                                            dict = hact$dict), 
                                   '% of cluster')))
  
  clust_chara$num_vars <- clust_chara$var_types %>% 
    filter(type_test == 'kruskal_test') %>% 
    .$variable
  
  ## analysis tables 
  
  clust_chara$analysis_tbl <- clust_ft$analysis_tbl
  
  ## recovery variables
  
  clust_chara$rec_vars <- c('sum_symptoms_long', 
                            'weight_loss_kg', 
                            'incomplete_covelescence', 
                            'new_medication_fup', 
                            'rehabilitation_fup_needed', 
                            'perf_impairment', 
                            'life_quality_score', 
                            'mental_health_score', 
                            'phq_anxiety_score', 
                            'phq_depression_score', 
                            'stress_score')

# descriptive stats -------
  
  insert_msg('Descriptive stats')
  
  clust_chara$desc_stats <- clust_chara$analysis_tbl %>% 
    future_map(~explore(.x, 
                        split_factor = 'clust_id', 
                        variables = clust_chara$var_types$variable, 
                        what = 'table', 
                        pub_styled = TRUE), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', names(globals$clust_colors)))
  
# testing ------
  
  insert_msg('Testing for differences between the clusters')
  
  clust_chara$test_results <- clust_chara$analysis_tbl %>% 
    future_map(~compare_variables(.x, 
                                  split_factor = 'clust_id', 
                                  variables = clust_chara$var_types$variable, 
                                  what = 'test', 
                                  types = clust_chara$var_types$type_test, 
                                  ci = FALSE, 
                                  pub_styled = TRUE, 
                                  adj_method = 'BH'), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(mutate, 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
# common result table ------
  
  insert_msg('Testing and stat table')
  
  clust_chara$summ_tbl <- map2(clust_chara$desc_stats, 
                               map(clust_chara$test_results, 
                                   select, 
                                   variable, 
                                   significance, 
                                   eff_size), 
                               left_join, by = 'variable')
  
# summary plots ------
  
  insert_msg('Summary plots, significance and effect size')
  
  clust_chara$summ_plots <- clust_chara$analysis_tbl %>% 
    future_map(~compare_variables(.x, 
                                  split_factor = 'clust_id', 
                                  variables = clust_chara$var_types$variable, 
                                  what = 'test', 
                                  types = clust_chara$var_types$type_test, 
                                  ci = FALSE, 
                                  pub_styled = FALSE, 
                                  adj_method = 'BH'), 
               .options = furrr_options(seed = TRUE))
  
  clust_chara$summ_plots <- clust_chara$summ_plots %>% 
    map(function(cohort) globals[c('demo_vars', 
                                   'clinic_vars', 
                                   'cov_vars', 
                                   'psych_var')] %>% 
          map(~filter(cohort, variable %in% .x))) %>% 
    unlist(recursive = FALSE) %>% 
    map(mutate, variable = exchange(variable, dict = hact$dict))
  
  clust_chara$summ_plots <- 
    list(x = clust_chara$summ_plots, 
         plot_title = rep(c('Baseline characteristic', 
                            'Medical history', 
                            'COVID-19 course and recovery', 
                            'Psychosocial rating'), 2), 
         plot_subtitle = c(rep('AT, survey study', 4), 
                           rep('IT, survey study', 4)), 
         point_color = list(c('gray60', globals$hact_colors[1]), 
                            c('gray60', globals$hact_colors[1]), 
                            c('gray60', globals$hact_colors[1]), 
                            c('gray60', globals$hact_colors[1]), 
                            c('gray60', globals$hact_colors[2]), 
                            c('gray60', globals$hact_colors[2]), 
                            c('gray60', globals$hact_colors[2]), 
                            c('gray60', globals$hact_colors[2]))) %>% 
    pmap(plot, 
         cust_theme = globals$common_theme, 
         show_labels = 'signif', 
         point_alpha = 0.8) %>% 
    map(~.x + 
          geom_hline(yintercept = -log10(0.05), linetype = 'dashed') + 
          labs(y = expression('Kruskal-Wallis or '*chi^2*' test, -log'[10]*' pFDR'), 
               y = expression('Effect size, '*eta^2*' or Cramer V')))
  
# Plots for single variables ----
  
  insert_msg('Plots for particular variables')
  
  clust_chara$plots <- 
    list(x = clust_chara$analysis_tbl, 
         y = globals$hact_labs, 
         z = clust_chara$test_results) %>% 
    pmap(function(x, y, z) list(variable = clust_chara$var_types$variable, 
                                type = clust_chara$var_types$type_plot, 
                                plot_title = paste(exchange(clust_chara$var_types$variable, 
                                                            dict = hact$dict), 
                                                   y, sep = ', '), 
                                plot_subtitle = z$plot_cap, 
                                y_lab = clust_chara$var_types$y_lab) %>% 
           future_pmap(plot_variable, 
                       x, 
                       split_factor = 'clust_id', 
                       scale = 'percent', 
                       cust_theme = globals$common_theme, 
                       x_lab = 'Cluster', 
                       x_n_labs = TRUE, 
                       .options = furrr_options(seed = TRUE)) %>% 
           set_names(clust_chara$var_types$variable))
  
  for(i in names(clust_chara$plots)) {
    
    clust_chara$plots[[i]][clust_chara$num_vars] <- 
      clust_chara$plots[[i]][clust_chara$num_vars] %>% 
      map(~.x + 
            scale_fill_manual(values = globals$clust_colors))
    
  }

# Ribbon plots with the min/max scaled recovery parameters in the clusters -----
  
  insert_msg('Ribbon plots')
  
  ## plotting tables

  clust_chara$ribbon_recovery <- clust_chara$analysis_tbl %>% 
    map(select, all_of(clust_chara$rec_vars)) %>% 
    map(~map_dfc(.x, as.numeric)) %>% 
    map(min_max) %>% 
    map2(., map(clust_chara$analysis_tbl, ~.x[, 'clust_id']), cbind) %>% 
    map(as_tibble)
  
  ## n numbers
  
  clust_chara$n_numbers <- clust_chara$ribbon_recovery %>% 
    map(count, clust_id)
  
  clust_chara$n_tags <- clust_chara$n_numbers %>% 
    map(~map2_chr(.x[[1]], .x[[2]], paste, sep = ': n = ')) %>% 
    map(paste, collapse = ', ') %>% 
    map(~paste0('\n', .))
  
  clust_chara$n_labs <- clust_chara$n_numbers %>% 
    map(~map2_chr(.x[[1]], .x[[2]], 
              paste, sep = ', n  = ')) %>% 
    map2(., clust_chara$n_numbers, 
         ~set_names(.x, .y[[1]]))
  
  ## labels of the axes with effect sizes and p values included
  
  clust_chara$ribbon_labs <- clust_chara$test_results %>% 
    map(filter, variable %in% clust_chara$rec_vars) %>% 
    map(mutate, 
        var_lab = exchange(variable, 
                           value = 'label_short', 
                           dict = hact$dict), 
        var_lab = stri_replace(var_lab, 
                               fixed = ' ', 
                               replacement = '\n'), 
        var_lab = paste(var_lab, 
                        eff_size, 
                        significance, 
                        sep = '\n'))
  
  clust_chara$ribbon_labs <- clust_chara$ribbon_labs %>% 
    map(~set_names(.x$var_lab, .x$variable))

  ## plots
  
  clust_chara$ribbon_recovery <- list(data = clust_chara$ribbon_recovery, 
                                      plot_subtitle = c('AT, survey study, min/max normalized', 
                                                        'IT, survey study, min/max normalized'), 
                                      plot_tag = clust_chara$n_tags) %>% 
    pmap(draw_stat_panel, 
         variables = clust_chara$rec_vars, 
         split_factor = 'clust_id', 
         stat = 'mean', 
         err_stat = '2se', 
         cust_theme = globals$common_theme, 
         form = 'line', 
         plot_title = 'Clinical and psychosocial CoV recovery')
  
  
  clust_chara$ribbon_recovery <- 
    list(x = clust_chara$ribbon_recovery, 
         y = clust_chara$ribbon_labs,
         z = clust_chara$n_labs) %>% 
    pmap(function(x, y, z) x + 
           scale_fill_manual(values = globals$clust_colors, 
                             labels = z, 
                             name = 'Cluster') + 
           scale_color_manual(values = globals$clust_colors, 
                              labels = z, 
                              name = 'Cluster') + 
           scale_y_discrete(labels = y, 
                            limits = clust_chara$rec_vars) + 
           scale_x_continuous(breaks = seq(0.1, 0.8, by = 0.1)) + 
           theme(axis.title = element_blank(), 
                 axis.line = element_blank(), 
                 axis.ticks = element_blank(), 
                 axis.text.y = element_blank()) + 
           coord_polar(theta = 'y', 
                       clip = 'off'))

  ## adding the breaks
  
  for(i in seq(0, 0.8, by = 0.2)) {
    
    clust_chara$ribbon_recovery$clust_north  <- 
      clust_chara$ribbon_recovery$clust_north + 
      annotate('text', 
               x = i, 
               y = 0.5, 
               label = as.character(i), 
               size = 2.75, 
               color = 'gray70')
    
    clust_chara$ribbon_recovery$clust_south  <- 
      clust_chara$ribbon_recovery$clust_south + 
      annotate('text', 
               x = i, 
               y = 0.5, 
               label = as.character(i), 
               size = 2.75, 
               color = 'gray70')
    
  }

# END -----
  
  plan('sequential')
  
  rm(i)
  
  insert_tail()