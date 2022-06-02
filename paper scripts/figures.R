# Main text figures 

  insert_head()
  
# container list -----
  
  figures <- list()
  
# Figure 1: CONSORT -------
  
  insert_msg('Figure 1: CONSORT')
  
  figures$consort <- plot_grid(ggdraw() + 
    draw_image('./study consort/consort_diagram.png')) %>% 
    as_figure('figure_1_consort', 
              w = 180, 
              h = 110)
  
# Figure 2: symptom recovery times -----
  
  insert_msg('Figure 2: symptom recovery times')
  
  ## emboldening the hyposmia and hypogeusia

  figures$sympt_recovery$upper_panel <- rec_dist$summ_plot_hact %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) embolden_scale(x, c('Hypo/anosmia', 'Hypo/ageusia'))) + 
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
    as_figure('figure_2_recovery_times', 
              w = 180, 
              h = 220)

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
              labels = c('A', '', 'B', ''), 
              label_size = 10) %>% 
    as_figure('figure_3_symptom_isolation', 
              w = 180, 
              h = 180)

# Figure 4: Apriori -----
  
  insert_msg('Figure 4: apriori')
  
  figures$apriori <- ap_sympt$conf_supp_plots[c('north.28', 
                                                'south.28', 
                                                'north.90', 
                                                'south.90')] %>% 
    map(~.x + 
          scale_color_gradient2(low = 'steelblue', 
                                mid = 'black', 
                                high = 'firebrick', 
                                midpoint = 4, 
                                limits = c(2.5, 6), 
                                name = 'Lift'))
  
  figures$apriori <- figures$apriori %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 'B'), 
              label_size = 10) %>% 
    plot_grid(get_legend(figures$apriori[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08)) %>% 
    as_figure('figure_4_apriori_analysis', 
              w = 180, 
              h = 190)
  
# Figure 5: clustering of the participants ------
  
  insert_msg('Figure 5: clustering')
  
  figures$clustering <- part_clust$feature_hm %>% 
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
    plot_grid(get_legend(part_clust$feature_hm[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08)) %>% 
    as_figure('figure_5_recovery_clusters', 
              w = 180, 
              h = 180)
  
# Figure 6: symptom duration in the recovery clusters -----
  
  insert_msg('Figure 6: Symptom duration in the recovery clusters')
  
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
    as_figure('figure_6_symptom_duration_clusters', 
              w = 180, 
              h = 180)
  
# Figure 7: baseline features in the recovery clusters -----  
  
  insert_msg('Figure 7: Baseline features in the recovery clusters')
  
  figures$base_clusters <- clust_chara$plots %>% 
    map(~.x[c('sex', 'comorb_present', 'daily_medication')]) %>% 
    transpose
  
  ## color adjustment
  
  figures$base_clusters$sex <- figures$base_clusters$sex %>% 
    map(~.x + 
          scale_fill_manual(values = c(male = 'steelblue', 
                                       female = 'coral3')))
  
  figures$base_clusters$comorb_present <- 
    figures$base_clusters$comorb_present %>% 
    map(~.x + scale_fill_manual(values = c(no = 'steelblue', yes = 'coral3')))
  
  figures$base_clusters$daily_medication <- 
    figures$base_clusters$daily_medication %>% 
    map(~.x + 
          scale_fill_manual(values = c('steelblue', 'cornsilk3', 'coral3')))
  
  ## panels
  
  figures$base_clusters$upper_panel <-  figures$base_clusters$sex %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    c(list(get_legend(figures$base_clusters$sex[[1]]))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr')
  
  figures$base_clusters$middle_panel <- figures$base_clusters$comorb_present %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    c(list(get_legend(figures$base_clusters$comorb_present[[1]]))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr')
  
  figures$base_clusters$bottom_panel <-  figures$base_clusters$daily_medication %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    c(list(get_legend(figures$base_clusters$daily_medication[[1]]))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr')
  
  ## entire figure
  
  figures$base_clusters <- plot_grid(figures$base_clusters$upper_panel, 
                                     figures$base_clusters$middle_panel, 
                                     figures$base_clusters$bottom_panel, 
                                     nrow = 3, 
                                     labels = LETTERS, 
                                     label_size = 10) %>% 
    as_figure('figure_7_baseline_clusters', 
              w = 180, 
              h = 210)
  
# Figure 8: recovery in the recovery clusters ------
  
  insert_msg('Figure 8: recovery in the recovery clusters')
  
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
    as_figure('figure_8_recovery_clusters', 
              w = 180, 
              h = 120)
  
# Saving the figures ------
  
  insert_msg('Saving the figures')
  
  figures %>% 
    walk(save_figure, 
         path = './paper/figures', 
         format = 'pdf', 
         device = cairo_pdf)

# END -----
  
  insert_tail()