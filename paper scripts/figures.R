# Main text figures 

  insert_head()
  
# container list -----
  
  figures <- list()
  
# Figure 1: symptom recovery times -----
  
  insert_msg('Figure 1: symptom recovery times')
  
  ## upper panel: median/IQR recovery times
  ## emboldening the hyposmia and hypogeusia

  figures$sympt_recovery$upper_panel <- rec_dist$summ_plot_hact %>% 
    map(embolden_elli, c('OD', 'Hypogeusia/ageusia')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv',
              axis = 'tblr')

  ## bottom panel: recovery curves for hyposmia and hypo/ageusia
  
  figures$sympt_recovery$bottom_panel <- kin_mod$plots_hact %>% 
    map(~.x[c('anosmia', 'taste_loss')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + 
          theme(plot.margin = ggplot2::margin(t = 5, r = 1, 
                                              b = 0, l = 0, unit = 'mm'))) %>% 
    plot_grid(plotlist = ., 
              ncol = 4, 
              align = 'hv', 
              axis = 'tblr')
  
  figures$sympt_recovery <- plot_grid(figures$sympt_recovery$upper_panel, 
                                      figures$sympt_recovery$bottom_panel, 
                                      nrow = 2, 
                                      rel_heights = c(0.7, 0.3), 
                                      labels = c('a', 'b'), 
                                      label_size = 10) %>% 
    as_figure('figure_1_recovery_times', 
              ref_name = 'sympt_recovery', 
              caption = paste('Symptom-specific recovery times in the', 
                              'ambulatory COVID-19 survey study.'), 
              w = 180, 
              h = 220)

# Figure 2: subjective and objective hyposmia -------
  
  insert_msg('Figure 2: subjective and objective hyposmia')
  
  figures$sniff_test <- 
    plot_grid(rater$hyposmia_rates$plots$`100` + 
                theme(legend.position = 'bottom'), 
              rater$kappa_forests$plots$fup100 + 
                labs(title = ''), 
              rater$hyposmia_rates$plots$`360` + 
                theme(legend.position = 'bottom'), 
              rater$kappa_forests$plots$fup360 + 
                labs(title = ''), 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('a', '', 'b', ''), 
              label_size = 10) %>% 
    as_figure('figure_2_sniffin_test', 
              ref_name = 'sniff_test', 
              caption = paste('Rates of subjective and objective hyposmia', 
                              'in the CovILD cohort two months', 
                              'and one year after COVID-19.'), 
              w = 180,
              h = 180)
  
# Figure 3: symptom distances -----
  
  insert_msg('Figure 3: symptom isolation')
  
  figures$sympt_mds <- sympt_dist$mds_plots_hact[c('north.long', 
                                                   'south.long', 
                                                   'north.pasc', 
                                                   'south.pasc')] %>% 
    map(~.x + 
          labs(tag = .x$labels$tag %>% 
                 stri_replace(regex = 'Obs.*\\nVariables:\\s{1}', 
                              replacement = ''))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('a', '', 'b', ''), 
              label_size = 10) %>% 
    as_figure('figure_3_symptom_isolation', 
              ref_name = 'sympt_mds', 
              caption = paste('Self-reported olfactory dysfunction and', 
                              'taste disorders are isolated persistent', 
                              'symptoms of COVID-19.'), 
              w = 180, 
              h = 180)

# Figure 4: symptom duration in the recovery clusters -----
  
  insert_msg('Figure 4: Symptom duration in the recovery clusters')
  
  figures$sympt_clusters <- clust_ft$ribbon_panels %>% 
    map2(., 
         paste0('Symptom recovery, ', c('AT', 'IT'), ', survey study'), 
         ~.x + 
           labs(title = .y) + 
           scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                                c('anosmia', 'taste_loss'), 
                                                                translate = TRUE), 
                            limits = globals$hact_symptom_order) + 
           theme(axis.text.y = element_markdown(), 
                 legend.position = 'none')) %>% 
    map(tag_to_sub, replace = TRUE) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(clust_ft$ribbon_panels[[1]] + 
                           guides(fill = FALSE) + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08)) %>% 
    as_figure('figure_4_symptom_duration_clusters', 
              ref_name = 'sympt_clusters', 
              caption = paste('Differing duration of neurocognitive and', 
                              'respiratory symptoms, fatigue, olfactory', 
                              'dysfunction and taste disorders defines', 
                              'the COVID-19 recovery clusters.'), 
              w = 180, 
              h = 180)
  
# Figure 5: recovery in the recovery clusters ------
  
  insert_msg('Figure 5: recovery in the recovery clusters')
  
  figures$recovery_clusters <- clust_chara$ribbon_recovery %>% 
    map2(., c('AT, survey study', 'IT, survey study'), 
         ~.x + 
           labs(title = .y) + 
           theme(plot.tag = element_blank(), 
                 plot.subtitle = element_blank())) %>% 
    plot_grid(plotlist = ., 
              nrow = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure('figure_5_recovery_clusters',
              ref_name = 'recovery_clusters', 
              caption = paste('Physical and mental health,', 
                              'and quality of life in', 
                              'the COVID-19 recovery clusters'), 
              w = 180, 
              h = 220)
  
# Saving the figures ------
  
  insert_msg('Saving the figures')
  
  figures %>% 
    walk(pickle, 
         path = './paper/figures', 
         format = 'pdf', 
         device = cairo_pdf)

  figures %>% 
    walk(pickle, 
         path = './paper/figures eps', 
         format = 'eps', 
         device = cairo_ps)
  
  figures %>% 
    walk(pickle, 
         path = './paper/figures png', 
         format = 'png', 
         type = 'cairo')
  
# END -----
  
  insert_tail()