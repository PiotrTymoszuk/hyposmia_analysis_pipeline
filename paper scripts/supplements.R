# supplementary figures 

  insert_head()
  
# container list -----
  
  suppl <- list()
  
# Figure S1: Frequency of symptoms, HACT -----
  
  insert_msg('Figure S1: HACT symptom frequency')
  
  suppl$hact_freq <- eda_sympt$hact_bubble %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                               c('anosmia', 'taste_loss'), 
                                                               translate = TRUE)) + 
          theme(axis.text.y = element_markdown())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure('figure_s1_hact_symp_frequency', 
              w = 180, 
              h = 190)
  
# Figure S2: HACT recovery symptoms -------
  
  insert_msg('Figure S2: fast-recovery symptoms')
  
  suppl$hact_recovery <- kin_mod$plots_hact %>% 
    map(~.x[c('fever', 'dim_appetite', 
              'joint_pain', 'muscle_pain', 
              'fatigue', 'breath_short')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + 
          scale_y_continuous(limits = c(0, 105), 
                             breaks = seq(0, 100, by = 25)) + 
          theme(plot.margin = ggplot2::margin(t = 5, r = 1, 
                                              b = 0, l = 0, unit = 'mm'))) %>% 
    plot_grid(plotlist = ., 
              ncol = 4, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 
                         'B', '', 
                         'C', '', 
                         'D', '', 
                         'E', '', 
                         'F', ''), 
              label_size = 10) %>% 
    as_figure('figure_s2_hact_kinetic', 
              w = 180, 
              h = 180)
  
# Figure S3: Frequency of symptoms, CovILD ------
  
  insert_msg('Figure S3: frequency of symptoms, CovILD')
  
  suppl$covild_freq <- eda_sympt$covild_bubble %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                               'anosmia_sympt', 
                                                               translate = TRUE, 
                                                               dict = covild$dict)) + 
          theme(axis.text.y = element_markdown())) %>% 
    plot_grid(plotlist = ., 
              nrow = 3, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure('figure_s3_covild_symp_frequency', 
              w = 180, 
              h = 220)
  
# Figure S4: kinetics of symptom resolution in the CovILD cohort -----
  
  insert_msg('Figure S4: CovILD kinetic')
  
  suppl$covild_recovery <- kin_mod$plots_covild %>% 
    map(~.x[c('anosmia_sympt', 
              'fatigue_sympt', 
              'dyspnoe_sympt')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', '', 
                         'B', '', '', 
                         'C', '', ''), 
              label_size = 10) %>% 
    as_figure('figure_s4_covild_kinetic', 
              w = 180, 
              h = 180)
  
# Figure S5: hyposmia rater, 3 months ----
  
  insert_msg('Figure S5: hyposmia rater, 3 month follow-up')
  
  suppl$hyposmia_rater100 <- rater$bubble_plots[c('fup100.cohort', 
                                                  'fup100.A', 
                                                  'fup100.HM', 
                                                  'fup100.HS')] %>% 
    map(~.x + 
          labs(subtitle = paste(.x$labels$subtitle, 
                                '% of complete observations', 
                                sep = '\n'))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure('figure_s5_hyposmia_rater100', 
              w = 180, 
              h = 160)
  
# Figure S6: hyposmia rater, 3 months ----
  
  insert_msg('Figure S6: hyposmia rater, 1-year follow-up')
  
  suppl$hyposmia_rater360 <- rater$bubble_plots[c('fup360.cohort', 
                                                  'fup360.A', 
                                                  'fup360.HM', 
                                                  'fup360.HS')] %>% 
    map(~.x + 
          labs(subtitle = paste(.x$labels$subtitle, 
                                '% of complete observations', 
                                sep = '\n'))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = LETTERS, 
              label_size = 10) %>% 
    as_figure('figure_s6_hyposmia_rater360', 
              w = 180, 
              h = 160)
  
# Figure S7: MDS for acute COVID-19 -----
  
  insert_msg('Figure S7: MDS for acute COVID-19')
  
  suppl$mds_acute <- sympt_dist$mds_plots_hact[c('north.acute', 
                                                 'south.acute')] %>% 
    map(~.x + 
          labs(tag = .x$labels$tag %>% 
                 stri_replace(regex = 'Obs.*\\nVariables:\\s{1}', 
                              replacement = ''))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure('figure_s7_mds_hact_acute_cov', 
              w = 180, 
              h = 100)
  
# Figure S8: cluster development -----
  
  insert_msg('Figure S8: Cluster development')
  
  suppl$clust_devel <- plot_grid(cl_devel$result_plot + 
                                   theme(legend.position = 'bottom'), 
                                 part_clust$diagnostic_plots$wss, 
                                 nrow = 2, 
                                 align = 'v', 
                                 axis = 'tblr', 
                                 rel_heights = c(0.6, 0.4), 
                                 labels = LETTERS, 
                                 label_size = 10) %>% 
    plot_grid(part_clust$imp_plot, 
              ncol = 2, 
              labels = c('', 'C'), 
              label_size = 10) %>% 
    as_figure('figure_s8_cluster_development', 
              w = 180, 
              h = 180)
  
# Figure S9: symptom count, phys.perf and perceived recovery, clusters -----
  
  insert_msg('Figure S9: Symptoms, performance and recovery in the clusters')
  
  suppl$cov_clusters <- clust_chara$plots %>% 
    map(~.x[c('sum_symptoms_acute', 
              'sum_symptoms_long')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 'B', ''), 
              label_size = 10) %>% 
    as_figure('figure_s9_cov_clusters', 
              w = 180, 
              h = 140)
  
  
# Figure S10: hyposmia and hypogeusia in the recovery clusters ------
  
  insert_msg('Figure S9: hyposmia and hypogeusia in the clusters')
  
  suppl$anosmia_clusters <- clust_ft$clust_ft_plots %>% 
    map(~.x[c('anosmia', 'taste_loss')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = .,
              ncol = 2, 
              align = 'hv',
              axis = 'tblr', 
              labels = c('A', '', 'B', ''), 
              label_size = 10) %>% 
    as_figure('figure_s10_hyposmia_clusters', 
              w = 180, 
              h = 120)
  
# Figure S11: duration of key symptoms in the recovery clusters -----
  
  insert_msg('Figure S11: fatigue, tiredness and tachypnea in the clusters')
  
  suppl$fatigue_clsters <- clust_ft$clust_ft_plots %>% 
    map(~.x[c('fatigue_day', 'fatigue', 'breath_short')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = .,
              ncol = 2, 
              align = 'hv',
              axis = 'tblr', 
              labels = c('A', '', 'B', '', 'C'), 
              label_size = 10) %>% 
    as_figure('figure_s11_fatigue_clusters', 
              w = 180, 
              h = 180)
  
# Figure S12: duration of neurocognitive symptoms in the recovery clusters -----
  
  insert_msg('Figure S12: neurocognitive features in the clusters')
  
  suppl$neurocog_clusters <- clust_ft$clust_ft_plots %>% 
    map(~.x[c('forgetfulness', 'imp_concentration', 'confusion')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = .,
              ncol = 2, 
              align = 'hv',
              axis = 'tblr', 
              labels = c('A', '', 'B', '', 'C'), 
              label_size = 10) %>% 
    as_figure('figure_s12_neurocognitive_clusters', 
              w = 180, 
              h = 180)

# Figure S13: physical performance, life quality and complete recovery, clusters -----
  
  insert_msg('Figure S13: performance, QoL and perceived recovery, clusters')
  
  suppl$rec_clusters$upper_panel <- clust_chara$plots %>% 
    map(~.x[c('perf_impairment', 
              'life_quality_score')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 'B', ''), 
              label_size = 10)
  
  suppl$rec_clusters$bottom_panel <- clust_chara$plots %>% 
    map(~.x$incomplete_covelescence + 
          theme(legend.position = 'none') + 
          scale_fill_manual(values = c(no = 'steelblue', 
                                       yes = 'coral3'))) %>% 
    c(list(get_legend(clust_chara$plots$clust_north$incomplete_covelescence + 
                        scale_fill_manual(values = c(no = 'steelblue', 
                                                     yes = 'coral3'))))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              rel_widths = c(1.3, 1.3, 0.4), 
              labels = 'C', 
              label_size = 10)
  
  suppl$rec_clusters <- plot_grid(suppl$rec_clusters$upper_panel, 
                                  suppl$rec_clusters$bottom_panel, 
                                  nrow = 2, 
                                  rel_heights = c(2, 1)) %>% 
    as_figure('figure_s13_recovery_clusters', 
              w = 180, 
              h = 210)
  
# Figure S14: psychosocial health in the clusters -----
  
  insert_msg('Figure S14: psychosocial health in the clusters')
  
  suppl$psych_clusters <- clust_chara$plots %>% 
    map(~.x[c('phq_anxiety_score', 
              'phq_depression_score', 
              'stress_score', 
              'mental_health_score')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 'B', '', 'C', '', 'D'), 
              label_size = 10) %>% 
    as_figure('figure_s14_psych_clusters', 
              w = 180, 
              h = 230)
  
# Saving the figures ------
  
  insert_msg('Saving the figures')
  
  suppl %>% 
    walk(save_figure, 
         path = './paper/supplementary figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END ----
  
  insert_tail()