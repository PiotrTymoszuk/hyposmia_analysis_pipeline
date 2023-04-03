# Characterized the clustering feature levels in the participant clusters.

  insert_head()
  
# container list ----
  
  clust_ft <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  ## analysis tables
  
  clust_ft$analysis_tbl <- part_clust[c('clust_north', 
                                        'clust_south')] %>% 
    map(~left_join(model.frame(.x) %>% 
                     rownames_to_column('ID'), 
                   .x$clust_assignment %>% 
                     mutate(ID = observation), 
                   by = 'ID')) %>% 
    map(as_tibble)
  
  ## appending them with the demographic, clinical and recovery features
  
  clust_ft$analysis_tbl <- map2(clust_ft$analysis_tbl, 
                                map(hact[c('north', 'south')], 
                                    select, 
                                    ID, 
                                    all_of(globals$demo_vars), 
                                    all_of(globals$clinic_vars), 
                                    all_of(globals$cov_vars), 
                                    all_of(globals$psych_var)), 
                                left_join, by = 'ID')
  
  ## n numbers
  
  clust_ft$n_numbers <- part_clust[c('clust_north', 
                                     'clust_south')] %>% 
    map(ngroups)
  
  clust_ft$n_tags <- clust_ft$n_numbers %>% 
    map(~map2_chr(.x[[1]], .x[[2]], paste, sep = ': n = ')) %>% 
    map(paste, collapse = ', ') %>% 
    map(~paste0('\n', .x))
  
  plan('multisession')
  
# Descriptive stats ----
  
  insert_msg('Descriptive stats')

  clust_ft$clust_ft_stats <- clust_ft$analysis_tbl %>% 
    future_map(~explore(.x, 
                        split_factor = 'clust_id', 
                        variables = globals$hact_symptoms, 
                        what = 'table', 
                        pub_styled = TRUE), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(reduce, left_join, by = 'variable') %>% 
    map(set_names, c('variable', 'clust_1', 'clust_2', 'clust_3'))
  
# Kruskal-Wallis tests ----
  
  insert_msg('Serial testing')
  
  clust_ft$clust_ft_test <- clust_ft$analysis_tbl %>% 
    future_map(~compare_variables(.x, 
                                  split_factor = 'clust_id', 
                                  variables = globals$hact_symptoms, 
                                  what = 'test', 
                                  types = 'kruskal_test', 
                                  ci = FALSE, 
                                  pub_styled = TRUE, 
                                  adj_method = 'BH'), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(mutate, plot_cap = paste(eff_size, significance, sep = ', '))
  
# Violin plots for all features -----
  
  insert_msg('Violin plots for all features')
  
  clust_ft$clust_ft_plots <- list(x = clust_ft$analysis_tbl, 
                                  y = globals$hact_labs, 
                                  z = clust_ft$clust_ft_test) %>% 
    pmap(function(x, y, z) list(variable = globals$hact_symptoms, 
                                plot_subtitle = z$plot_cap, 
                                plot_title = translate_var(globals$hact_symptoms, 
                                                           dict = hact$dict) %>% 
                                  paste(y, sep = ', ')) %>% 
           pmap(plot_variable, 
                x, 
                split_factor = 'clust_id', 
                type = 'violin', 
                x_lab = 'Cluster', 
                y_lab = 'Recovery time, days post CoV', 
                point_hjitter = 2, 
                cust_theme = globals$common_theme) %>% 
           map(~.x + 
                 scale_fill_manual(values = globals$clust_colors) + 
                 scale_x_discrete(limits = c('STDR', 'RR', 'SR')) + 
                 labs(tag = .x$labels$tag %>% 
                        stri_replace_all(fixed = '\n', replacement = ', ') %>% 
                        paste0('\n', .))) %>% 
           set_names(globals$hact_symptoms))
  
# Summary plots: significance versus effect size ------
  
  insert_msg('Plots with summary test results')
  
  clust_ft$clust_ft_summ <- clust_ft$analysis_tbl %>% 
    future_map(~compare_variables(.x, 
                                  split_factor = 'clust_id', 
                                  variables = globals$hact_symptoms, 
                                  what = 'test', 
                                  types = 'kruskal_test', 
                                  ci = FALSE, 
                                  pub_styled = FALSE, 
                                  adj_method = 'BH'), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(arrange, p_adjusted) %>% 
    map(~mutate(.x, 
                top_rank = 1:nrow(.x), 
                variable = translate_var(variable, dict = hact$dict), 
                plot_lab = ifelse(top_rank %in% 1:10, variable, NA)))
  
  clust_ft$clust_ft_summ <- list(x = clust_ft$clust_ft_summ, 
                                 plot_subtitle = c('AT, survey study', 
                                                   'IT, survey study'), 
                                 point_color = list(c('gray60', globals$hact_colors[1]), 
                                                    c('gray60', globals$hact_colors[2]))) %>% 
    pmap(plot, 
         cust_theme = globals$common_theme, 
         show_labels = 'none', 
         plot_title = 'Symptom recovery') %>% 
    map(~.x + 
          geom_text_repel(aes(label = plot_lab), 
                          size = 2.5) + 
          geom_hline(yintercept = -log10(0.05), 
                     linetype = 'dashed') + 
          scale_x_continuous(limits = c(0, 0.55), 
                             breaks = seq(0, 0.5, by = 0.1)) + 
          scale_y_continuous(limits = c(0, 55), 
                             breaks = seq(0, 50, by = 10)) + 
          labs(x = expression('Effect size, '*eta^2), 
               y = expression(chi^2*' test, -log'[10]*' pFDR')))
  
# Special representation: ribbon plots with the symptom  frequencies -----
  
  insert_msg('Ribbon plots with the symptom frequencies')
  
  clust_ft$ribbon_panels <- list(data = clust_ft$analysis_tbl, 
                                 plot_subtitle = c('AT, survey cohort', 
                                                   'IT, survey cohort'), 
                                 plot_tag = clust_ft$n_tags) %>% 
    pmap(draw_stat_panel, 
         variables = globals$hact_symptoms, 
         split_factor = 'clust_id', 
         stat = 'mean', 
         err_stat = '2se', 
         form = 'line', 
         plot_title = 'Symptom recovery', 
         x_lab = 'Mean symptom recovery, days post CoV', 
         cust_theme = globals$common_theme, 
         alpha = 0.2) %>% 
    map(~.x + 
          theme(axis.title.y = element_blank()) + 
          scale_fill_manual(values = globals$clust_colors, 
                            name = 'Cluster') + 
          scale_color_manual(values = globals$clust_colors, 
                             name = 'Cluster') + 
          scale_y_discrete(labels = translate_var(globals$hact_symptoms, 
                                                  dict = hact$dict), 
                           limits = globals$hact_symptom_order))

# END -----
  
  plan('sequential')
  
  insert_tail()