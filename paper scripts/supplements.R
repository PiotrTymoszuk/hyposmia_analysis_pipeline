# supplementary figures 

  insert_head()
  
# container list -----
  
  suppl <- list()
  
# Figure S1: CONSORT ------- 
  
  insert_msg('Figure S1: CONSORT diagram')

  suppl$consort <- plot_grid(ggdraw() + 
                                 draw_image('./study consort/consort_diagram.png')) %>% 
    as_figure('figure_s1_consort', 
              ref_name = 'consort', 
              caption = 'Flow diagram of the analysis inclusion process for the longitudinal CovILD cohort and the Health after COVID-19 survey study.', 
              w = 180, 
              h = 110)
    
# Figure S2: Frequency of symptoms, HACT -----
  
  insert_msg('Figure S2: HACT symptom frequency')
  
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
    as_figure('figure_s2_hact_symp_frequency', 
              ref_name = 'hact_freq', 
              caption = 'Frequency of COVID-19 symptoms in the survey study.', 
              w = 180, 
              h = 190)
  
# Figure S3: HACT recovery symptoms -------
  
  insert_msg('Figure S3: fast-recovery symptoms')
  
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
    as_figure('figure_s3_hact_kinetic', 
              ref_name = 'hact_recovery', 
              caption = 'Kinetic of recovery from leading acute COVID-19 symptoms in the survey study.', 
              w = 180, 
              h = 180)
  
# Figure S4: Frequency of symptoms, CovILD ------
  
  insert_msg('Figure S4: frequency of symptoms, CovILD')
  
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
    as_figure('figure_s4_covild_symp_frequency', 
              ref_name = 'covild_freq', 
              caption = 'Symptom frequency in ambulatory, moderate and severe COVID-19 subsets of the CovILD study.', 
              w = 180, 
              h = 220)
  
# Figure S5: kinetics of symptom resolution in the CovILD cohort -----
  
  insert_msg('Figure S5: CovILD kinetic')
  
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
    as_figure('figure_s5_covild_kinetic', 
              ref_name = 'covild_recovery', 
              caption = 'Kinetic of recovery from olfactory dysfunction, reduced performance and dyspnea in ambulatory, moderate and severe COVID-19 subsets of the CovILD study.', 
              w = 180, 
              h = 180)
  
# Figure S6: hyposmia rater, 3 months ----
  
  insert_msg('Figure S6: hyposmia rater, 3 month follow-up')
  
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
    as_figure('figure_s6_hyposmia_rater100', 
              re_name = 'hyposmia_rater100', 
              caption = 'Rates of self-reported olfactory dysfunction and olfactory dysfunction in the sniffing stick test at 3-month post COVID-19 follow-up in the ambulatory, moderate and severe COVID-19 subsets of the CovILD study.', 
              w = 180, 
              h = 160)
  
# Figure S7: hyposmia rater, 3 months ----
  
  insert_msg('Figure S7: hyposmia rater, 1-year follow-up')
  
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
    as_figure('figure_s7_hyposmia_rater360', 
              ref_name = 'hyposmia_rater360', 
              caption = 'Rates of self-reported olfactory dysfunction and olfactory dysfunction in the sniffing stick test at 1-year post COVID-19 follow-up in the ambulatory, moderate and severe COVID-19 subsets of the CovILD study.', 
              w = 180, 
              h = 160)
  
# Figure S8: MDS for acute COVID-19 -----
  
  insert_msg('Figure S8: MDS for acute COVID-19')
  
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
    as_figure('figure_s8_mds_hact_acute_cov', 
              ref_name = 'mds_acute', 
              caption = 'Multi-dimensional scaling analysis of acute COVID-19 symptoms in the survey study.', 
              w = 180, 
              h = 100)
  
# Figure S9: cluster development -----
  
  insert_msg('Figure S9: Cluster development')
  
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
    as_figure('figure_s9_cluster_development', 
              ref_name = 'clust_devel', 
              caption = 'Definition of the COVID-19 recovery clusters and clustering feature importance in the survey study.', 
              w = 180, 
              h = 180)
  
# Figure S10: clustering of the participants ------
  
  insert_msg('Figure S10: clustering')
  
  suppl$clustering <- part_clust$feature_hm %>% 
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
    as_figure('figure_s10_recovery_clusters', 
              ref_name = 'clustering', 
              caption = 'Clustering of ambulatory COVID-19 individuals by symptom-specific recovery times.', 
              w = 180, 
              h = 180)  
  
# Figure S11: symptom count, clusters -----
  
  insert_msg('Figure S11: Symptom count in the clusters')
  
  suppl$cov_clusters <- clust_chara$plots %>% 
    map(~.x[c('sum_symptoms_acute', 
              'sum_symptoms_long')]) %>% 
    transpose %>% 
    unlist(recursive = FALSE) %>% 
    map2(.,
         c('# symptoms, 0 - 14 days, AT', 
           '# symptoms, 0 - 14 days, IT', 
           '# symptoms, 28 days, AT', 
           '# symptoms, 28 days, IT'), 
         ~.x +
           labs(title = .y) + 
           theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              labels = c('A', '', 'B', ''), 
              label_size = 10) %>% 
    as_figure('figure_s11_cov_clusters', 
              ref_name = 'cov_clusters', 
              caption = 'Numbers of COVID-19 symptoms in the survey study recovery clusters.', 
              w = 180, 
              h = 140)

# Figure S12: baseline features in the recovery clusters -----  
  
  insert_msg('Figure S12: Baseline features in the recovery clusters')
  
  suppl$base_clusters <- clust_chara$plots %>% 
    map(~.x[c('sex', 'comorb_present', 'daily_medication')]) %>% 
    transpose
  
  ## color adjustment
  
  suppl$base_clusters$sex <- suppl$base_clusters$sex %>% 
    map(~.x + 
          scale_fill_manual(values = c(male = 'steelblue', 
                                       female = 'coral3')))
  
  suppl$base_clusters$comorb_present <- 
    suppl$base_clusters$comorb_present %>% 
    map(~.x + scale_fill_manual(values = c(no = 'steelblue', yes = 'coral3')))
  
  suppl$base_clusters$daily_medication <- 
    suppl$base_clusters$daily_medication %>% 
    map(~.x + 
          scale_fill_manual(values = c('steelblue', 'cornsilk3', 'coral3')))
  
  ## panels
  
  suppl$base_clusters$upper_panel <-  suppl$base_clusters$sex %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    c(list(get_legend(suppl$base_clusters$sex[[1]]))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr')
  
  suppl$base_clusters$middle_panel <- suppl$base_clusters$comorb_present %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    c(list(get_legend(suppl$base_clusters$comorb_present[[1]]))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr')
  
  suppl$base_clusters$bottom_panel <-  suppl$base_clusters$daily_medication %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    c(list(get_legend(suppl$base_clusters$daily_medication[[1]]))) %>% 
    plot_grid(plotlist = ., 
              ncol = 3, 
              align = 'hv', 
              axis = 'tblr')
  
  ## entire figure
  
  suppl$base_clusters <- plot_grid(suppl$base_clusters$upper_panel, 
                                   suppl$base_clusters$middle_panel, 
                                   suppl$base_clusters$bottom_panel, 
                                   nrow = 3, 
                                   labels = LETTERS, 
                                   label_size = 10) %>% 
    as_figure('figure_s12_baseline_clusters', 
              ref_name = 'base_clusters', 
              caption = 'COVID-19 recovery clusters differ in sex distribution, comorbidity and daily medication rates.', 
              w = 180, 
              h = 210)
  
# Saving the figures ------
  
  insert_msg('Saving the figures')
  
  suppl %>% 
    walk(pickle, 
         path = './paper/supplementary figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END ----
  
  insert_tail()