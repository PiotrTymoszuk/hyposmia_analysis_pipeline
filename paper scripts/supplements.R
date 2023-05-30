# supplementary figures 

  insert_head()
  
# container list -----
  
  suppl <- list()
  
# Figure S1: CONSORT ------- 
  
  insert_msg('Figure S1: CONSORT diagram')

  suppl$consort <- 
    plot_grid(ggdraw() + 
                draw_image('./study consort/consort_diagram.png')) %>% 
    as_figure('figure_s1_consort', 
              ref_name = 'consort', 
              caption = paste('Flow diagram of the analysis inclusion', 
                              'process for the longitudinal CovILD cohort', 
                              'and the Health after COVID-19 survey study.'), 
              w = 180, 
              h = 110)
  
# Figure S2: sample size -------
  
  insert_msg('Figure S2: sample size')
  
  suppl$sample_size <- 
    plot_grid(eda_size$plot) %>% 
    as_figure('figure_s2_sample_size', 
              ref_name = 'sample_size', 
              caption = paste('Estimation of sample size for clustering', 
                              'anlysis with the survey study datasets.'), 
              w = 130, 
              h = 110)
  
# Figure S3: top differences between the HACT cohorts ------
  
  insert_msg('Figure S3: top differences between the HACT cohorts')
  
  suppl$hact_differences <- 
    list(eda_cohort$plots_hact$obs_time + 
           theme(legend.position = 'none'), 
         eda_cohort$plots_hact$cov_outbreak, 
         eda_cohort$plots_hact$bmi_class_before, 
         eda_cohort$plots_hact$daily_medication, 
         eda_cohort$plots_hact$two_plus_infections_antibiotics, 
         eda_cohort$plots_hact$phq_depression_score + 
           theme(legend.position = 'none'), 
         eda_cohort$plots_hact$phq_anxiety_score + 
           theme(legend.position = 'none'), 
         eda_cohort$plots_hact$life_quality_score + 
           theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv',
              axis = 'tblr') %>% 
    as_figure('figure_s3_top_differences_hact', 
              ref_name = 'hact_differences', 
              caption = paste('The largest significant diffeferences in', 
                              'demographic, clinical and recovery variables', 
                              'between the Austria and Italy cohorts', 
                              'of the survey study.'), 
              w = 180, 
              h = 220)
    
# Figure S4: Frequency of symptoms, HACT -----
  
  insert_msg('Figure S4: HACT symptom frequency')
  
  suppl$hact_freq <- eda_sympt$hact_bubble %>% 
    map2(., paste0('Symptom frequency, ', c('AT', 'IT'), ', survey study'), 
         ~.x + 
           labs(title = .y, 
                subtitle = .x$labels$subtitle %>% 
                  stri_replace(regex = '^(AT|IT),\\s{1}', 
                               replacement = '')) + 
          scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                               c('anosmia', 'taste_loss'), 
                                                               translate = TRUE)) + 
          theme(axis.text.y = element_markdown())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure('figure_s4_hact_symp_frequency', 
              ref_name = 'hact_freq', 
              caption = 'Frequency of COVID-19 symptoms in the survey study.', 
              w = 180, 
              h = 190)
  
# Figure S5: differences in symptoms between the HACT cohorts ----
  
  insert_msg('Figures S5: differences in symptoms, HACT')
  
  suppl$hact_signif_sympt <- 
    plot_grid(eda_sympt$signif_hact$plots$acute + 
                theme(legend.position = 'none'), 
              plot_grid(eda_sympt$signif_hact$plots$long + 
                          theme(legend.position = 'none'), 
                        get_legend(eda_sympt$signif_hact$plots[[1]] + 
                                     theme(legend.position = 'bottom')), 
                        nrow = 2, 
                        rel_heights = c(0.6, 0.4)), 
              ncol = 2) %>% 
    as_figure('figure_s5_significant_symptoms_hact', 
              ref_name = 'hact_signif_sympt',
              caption = paste('Significant differences in frequancy of', 
                              'COVID-19 symptoms in the survey study.'), 
              w = 180, 
              h = 110)

# Figure S6: HACT recovery symptoms -------
  
  insert_msg('Figure S6: fast-recovery symptoms')
  
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
              labels = c('a', '', 
                         'b', '', 
                         'c', '', 
                         'd', '', 
                         'e', '', 
                         'f', ''), 
              label_size = 10) %>% 
    as_figure('figure_s6_hact_kinetic', 
              ref_name = 'hact_recovery', 
              caption = paste('Kinetic of recovery from leading', 
                              'acute COVID-19 symptoms in the survey study.'), 
              w = 180, 
              h = 180)
  
# Figure S7: Frequency of symptoms, CovILD ------
  
  insert_msg('Figure S7: frequency of symptoms, CovILD')
  
  suppl$covild_freq <- eda_sympt$covild_bubble %>% 
    map2(., 
         paste('Symptom frequency,', 
               c('ambulatory', 'moderate', 'severe'), 
               'COVID-19'), 
         ~.x + 
           labs(title = .y, 
                subtitle = .x$labels$subtitle %>% 
                  stri_replace(regex = '^.*\\s{1}CoV,\\s{1}', 
                               replacement = '')) +
           scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                                'anosmia_sympt', 
                                                                translate = TRUE, 
                                                                dict = covild$dict)) + 
           theme(axis.text.y = element_markdown())) %>% 
    plot_grid(plotlist = ., 
              nrow = 3, 
              align = 'hv', 
              axis = 'tblr') %>% 
    as_figure('figure_s7_covild_symp_frequency', 
              ref_name = 'covild_freq', 
              caption = paste('Symptom frequency in ambulatory,', 
                              'moderate and severe COVID-19 subsets', 
                              'of the CovILD study.'), 
              w = 180, 
              h = 220)
         
# Figure S8: kinetics of symptom resolution in the CovILD cohort -----
  
  insert_msg('Figure S8: CovILD kinetic')
  
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
              labels = c('a', '', '', 
                         'b', '', '', 
                         'c', '', ''), 
              label_size = 10) %>% 
    as_figure('figure_s8_covild_kinetic', 
              ref_name = 'covild_recovery', 
              caption = paste('Kinetic of recovery from olfactory', 
                              'dysfunction, reduced performance and dyspnea', 
                              'in ambulatory, moderate and severe COVID-19', 
                              'subsets of the CovILD study.'), 
              w = 180, 
              h = 180)
  
# Figure S9: hyposmia rater, 3 months ----
  
  insert_msg('Figure S9: hyposmia rater, 3 month follow-up')
  
  ## upper panel: confusion matrices
  ## bottom panel: ROC plots
  ## the entire figure
  
  suppl$hyposmia_rater100 <- 
    list(upper = rater$confusion_plots$plots[c('fup100.cohort', 
                                               'fup100.A', 
                                               'fup100.HM', 
                                               'fup100.HS')], 
         lower = rater$roc$plots[c('fup100.cohort', 
                                   'fup100.A', 
                                   'fup100.HM', 
                                   'fup100.HS')]) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 2, 
                   align = 'hv', 
                   axis = 'tblr')) %>% 
    plot_grid(plotlist = ., 
              nrow = 2, 
              labels = letters, 
              label_size = 10) %>% 
    as_figure('figure_s9_hyposmia_rater100', 
              ref_name = 'hyposmia_rater100', 
              caption = paste('Rates of self-reported olfactory', 
                             'dysfunction and olfactory dysfunction', 
                             'in the Sniffin Stick Test at 3-month post', 
                             'COVID-19 follow-up in the ambulatory,', 
                             'moderate and severe COVID-19 subsets', 
                             'of the CovILD study.'),
              w = 160, 
              h = 210)
  
# Figure S10: hyposmia rater, 12 months -------
  
  insert_msg('Figure S10: hyposmia rater, 1-year follow-up')
  
  ## upper panel: confusion matrices
  ## bottom panel: ROC plots
  ## the entire figure
  
  suppl$hyposmia_rater360 <- 
    list(upper = rater$confusion_plots$plots[c('fup360.cohort', 
                                               'fup360.A', 
                                               'fup360.HM', 
                                               'fup360.HS')], 
         lower = rater$roc$plots[c('fup360.cohort', 
                                   'fup360.A', 
                                   'fup360.HM', 
                                   'fup360.HS')]) %>% 
    map(~plot_grid(plotlist = .x, 
                   ncol = 2, 
                   align = 'hv', 
                   axis = 'tblr')) %>% 
    plot_grid(plotlist = ., 
              nrow = 2, 
              labels = letters, 
              label_size = 10) %>% 
    as_figure('figure_s10_hyposmia_rater360', 
              ref_name = 'hyposmia_rater360', 
              caption = paste('Rates of self-reported olfactory', 
                              'dysfunction and olfactory dysfunction', 
                              'in the Sniffin Stick Test at 1-year post', 
                              'COVID-19 follow-up in the ambulatory,', 
                              'moderate and severe COVID-19 subsets', 
                              'of the CovILD study.'),
              w = 160, 
              h = 210)
  
# Figure S11: kinetics of subjective and objective hyposmia ------
  
  insert_msg('Figure S11: kinetics of subjective and objective OD')
  
  suppl$od_kinetic <- 
    c(sst_kinet$plots, 
      od_kinet$alluvial_plots$plots["objective"]) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr', 
              rel_heights = c(1, 1.1), 
              labels = c('a', '', 'b', ''), 
              label_size = 10) %>% 
    as_figure('figure_s11_objective_od_kinetic', 
              ref_name = 'od_kinetic', 
              caption = paste('Individuals trajectories of objective', 
                              'olfactory dysfunction in the CovILD study', 
                              'subset with the complete longitudinal', 
                              'follow-up data.'), 
              w = 160, 
              h = 160)
  
# Figure S12: MDS for acute COVID-19 -----
  
  insert_msg('Figure S12: MDS for acute COVID-19')
  
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
    as_figure('figure_s12_mds_hact_acute_cov', 
              ref_name = 'mds_acute', 
              caption = paste('Multi-dimensional scaling analysis of',
                              'acute COVID-19 symptoms in the survey study.'), 
              w = 180, 
              h = 100)
  
# Figure S13: apriori -----
  
  insert_msg('Figure S13: apriori')
  
  suppl$apriori <- ap_sympt$bubble_plots[c('north.28', 
                                           'south.28', 
                                           'north.90', 
                                           'south.90')] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              rel_heights = c(0.78, 0.22), 
              axis = 'tblr', 
              labels = c('a', '', 'b'), 
              label_size = 10) %>% 
    plot_grid(get_legend(ap_sympt$bubble_plots[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08)) %>% 
    as_figure('figure_s13_apriori_analysis', 
              ref_name = 'apriori', 
              caption = paste('Co-occurrence of self-reported olfactory', 
                              'dysfunction and other symptoms in post-acute', 
                              'COVID-19 sequelae.'), 
              w = 180, 
              h = 170)
  
# Figure S14: cluster development -----
  
  insert_msg('Figure S14: Cluster development')
  
  suppl$clust_devel <- plot_grid(cl_devel$result_plot + 
                                   theme(legend.position = 'bottom'), 
                                 part_clust$diagnostic_plots$wss, 
                                 nrow = 2, 
                                 align = 'v', 
                                 axis = 'tblr', 
                                 rel_heights = c(0.6, 0.4), 
                                 labels = letters, 
                                 label_size = 10) %>% 
    plot_grid(clust_imp$plot + 
                scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                                     c('anosmia', 'taste_loss'), 
                                                                     translate = TRUE)) + 
                theme(axis.text.y = element_markdown()), 
              ncol = 2, 
              labels = c('', 'c'), 
              label_size = 10) %>% 
    as_figure('figure_s14_cluster_development', 
              ref_name = 'clust_devel', 
              caption = paste('Definition of the COVID-19 recovery', 
                              'clusters and clustering feature', 
                              'importance in the survey study.'), 
              w = 180, 
              h = 180)
  
# Figure S15: clustering of the participants ------
  
  insert_msg('Figure S15: clustering')
  
  ## upper panel: clustering variance and n number distribution
  
  suppl$clustering$upper <- part_clust[c("variance_plot", "n_plot")] %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv',
              axis = 'tblr', 
              labels = letters, 
              label_size = 10)
  
  ## bottom panel: heat map of the recovery times
  
  suppl$clustering$bottom <- part_clust$feature_hm %>% 
    map(~.x + labs(subtitle = NULL)) %>% 
    map(tag_to_sub, replace = TRUE) %>% 
    map2(., c('Recovery time, AT, survey study', 
              'Recovery time, IT, survey study'), 
         ~.x + 
           labs(title = .y) + 
           scale_y_discrete(labels = function(x) embolden_scale(x, 
                                                                c('anosmia', 'taste_loss'), 
                                                                translate = TRUE), 
                            limits = globals$hact_symptom_order) + 
           theme(axis.text.y = element_markdown(), 
                 legend.position = 'none', 
                 plot.tag = element_blank())) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              axis = 'tblr') %>% 
    plot_grid(get_legend(part_clust$feature_hm[[1]] + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.92, 0.08))
  
  ## the entire figure 
  
  suppl$clustering <- plot_grid(suppl$clustering$upper, 
                                suppl$clustering$bottom,
                                nrow = 2, 
                                rel_heights = c(0.2, 0.8),
                                labels = c('', 'c'), 
                                label_size = 10) %>% 
    as_figure('figure_s15_recovery_clusters', 
              ref_name = 'clustering', 
              caption = paste('Clustering of ambulatory COVID-19 individuals', 
                              'in the survey study', 
                              'by symptom-specific recovery times.'), 
              w = 180, 
              h = 220)  
  
# Figure S16: symptom count, clusters -----
  
  insert_msg('Figure S16: Symptom count in the clusters')
  
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
              labels = c('a', '', 'b', ''), 
              label_size = 10) %>% 
    as_figure('figure_s16_cov_clusters', 
              ref_name = 'cov_clusters', 
              caption = paste('Numbers of COVID-19 symptoms in the', 
                              'survey study recovery clusters.'), 
              w = 180, 
              h = 140)

# Figure S17: baseline features in the recovery clusters -----  
  
  insert_msg('Figure S17: Baseline features in the recovery clusters')
  
  
  ## color adjustment, bundling the plots for the cohorts
  ## the entire figure
  
  suppl$base_clusters <- clust_chara$plots %>% 
    map(~.x[c('sex', 'comorb_present', 'daily_medication')]) %>% 
    transpose %>%  
    map(~map(.x, ~.x + scale_fill_brewer(palette = 'Reds'))) %>% 
    map(two_panel, legend.position = 'bottom')
  
  suppl$base_clusters$age <- clust_chara$plots %>% 
    map(~.x$age) %>% 
    map(~.x + theme(legend.position = 'none')) %>% 
    two_panel(legend.position = 'none')
  
  suppl$base_clusters <- 
    suppl$base_clusters[c("age", "sex", "comorb_present", "daily_medication")]
  
  suppl$base_clusters <- suppl$base_clusters %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              labels = letters, 
              label_size = 10) %>% 
    as_figure('figure_s17_baseline_clusters', 
              ref_name = 'base_clusters', 
              caption = paste('COVID-19 recovery clusters of the', 
                              'survey study differ in, ', 
                              'age, sex distribution, comorbidity and daily', 
                              'medication rates.'), 
              w = 195, 
              h = 170)
  
# Saving the figures ------
  
  insert_msg('Saving the figures')
  
  suppl %>% 
    walk(pickle, 
         path = './paper/supplementary figures', 
         format = 'pdf', 
         device = cairo_pdf)
  
# END ----
  
  insert_tail()