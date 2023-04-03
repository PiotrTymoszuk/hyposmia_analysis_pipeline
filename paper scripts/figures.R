# Main text figures 

  insert_head()
  
# container list -----
  
  figures <- list()
  
# Figure 1: symptom recovery times -----
  
  insert_msg('Figure 1: symptom recovery times')
  
  ## emboldening the hyposmia and hypogeusia

  figures$sympt_recovery$upper_panel <- rec_dist$summ_plot_hact %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) embolden_scale(x, c('OD', 'Hypogeusia/ageusia'))) + 
          theme(axis.text.y = element_markdown())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv',
              axis = 'tblr')
  
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
                                      labels = c('A', 'B'), 
                                      label_size = 10) %>% 
    as_figure('figure_1_recovery_times', 
              ref_name = 'sympt_recovery', 
              caption = 'Symptom-specific recovery times in the ambulatory COVID-19 survey study.', 
              w = 180, 
              h = 220)

# Figure 2: symptom distances -----
  
  insert_msg('Figure 2: symptom isolation')
  
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
              labels = c('A', '', 'B', ''), 
              label_size = 10) %>% 
    as_figure('figure_2_symptom_isolation', 
              ref_name = 'sympt_mds', 
              caption = 'Self-reported olfactory dysfunction and taste disorders are isolated persistent symptoms of COVID-19.', 
              w = 180, 
              h = 180)

# Figure 3: Apriori -----
  
  insert_msg('Figure 3: apriori')

  figures$apriori <- ap_sympt$bubble_plots[c('north.28', 
                                             'south.28', 
                                             'north.90', 
                                             'south.90')] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              rel_heights = c(0.78, 0.22), 
              axis = 'tblr', 
              labels = c('A', '', 'B'), 
              label_size = 10) %>% 
    plot_grid(get_legend(ap_sympt$bubble_plots[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08)) %>% 
    as_figure('figure_3_apriori_analysis', 
              ref_name = 'apriori', 
              caption = 'Co-occurrence of self-reported olfactory dysfunction and other symtoms in post-acute COVID-19 sequelae.', 
              w = 180, 
              h = 170)
  
# Figure 4: symptom duration in the recovery clusters -----
  
  insert_msg('Figure 4: Symptom duration in the recovery clusters')
  
  figures$sympt_clusters <- clust_ft$ribbon_panels %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                               c('anosmia', 'taste_loss'), 
                                                               translate = TRUE), 
                           limits = globals$hact_symptom_order) + 
          theme(axis.text.y = element_markdown(), 
                legend.position = 'none')) %>% 
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
              caption = 'Differing duration of neurocognitive and respiratory symptoms, fatigue, olfactory dysfunction and taste disorders defines the COVID-19 recovery clusters.', 
              w = 180, 
              h = 180)
  
# Figure 5: recovery in the recovery clusters ------
  
  insert_msg('Figure 5: recovery in the recovery clusters')
  
  figures$recovery_clusters <- clust_chara$ribbon_recovery %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(clust_chara$ribbon_recovery[[1]] + 
                           theme(legend.position = 'bottom') + 
                           guides(fill = FALSE)), 
              nrow = 2, 
              rel_heights = c(0.85, 0.15)) %>% 
    as_figure('figure_5_recovery_clusters', 
              caption = ' Differing duration of neurocognitive and respiratory symptoms, fatigue, olfactory dysfunction and taste disorders defines the COVID-19 recovery clusters.', 
              w = 180, 
              h = 120)
  
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
  
# END -----
  
  insert_tail()