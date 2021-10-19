# This script generates paper figures 

  insert_head()
  
# data containers -----
  
  paper_figures <- list()
  suppl_figures <- list()
  
# Figure 1: CONSORT ----
  
  insert_msg('Figure 1: CONSORT diagrams')
  
  paper_figures$consort <- plot_grid(ggdraw() + 
                                       draw_image('./study consort/consort_diagram.png')) %>% 
    as_figure_object(figure_label = 'figure_1_consort', 
                     w = 180, 
                     h = 100)
  
# Figure 2: COVID-19 symptom presence and resolution time -----
  
  insert_msg('Figure 2: symptom presence and resolution time')
  
  paper_figures$kinetic_modeling <- plot_grid(plot_grid(kinet$lrt_summ_plots$survey$survey_at + 
                                                          theme(plot.subtitle = element_blank()) + 
                                                          labs(title = 'HACT: Austria') + 
                                                          expand_limits(x = 320), 
                                                        kinet$lrt_summ_plots$survey$survey_it + 
                                                          theme(plot.subtitle = element_blank()) + 
                                                          labs(title = 'HACT: Italy') + 
                                                          expand_limits(x = 320), 
                                                        ncol = 2, 
                                                        align = 'hv'), 
                                              plot_grid(rec_time$duration_plots$survey$survey_at + 
                                                          scale_x_continuous(limits = c(0, 100)),
                                                        rec_time$duration_plots$survey$survey_it + 
                                                          scale_x_continuous(limits = c(0, 100)), 
                                                        ncol = 2, 
                                                        align = 'hv'), 
                                              nrow = 2, 
                                              rel_heights = c(0.45, 0.55), 
                                              labels = LETTERS, 
                                              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_2_kinetic_modeling', 
                     w = 180, 
                     h = 165)
  
# Figure 3: Kinetic plots for self-reported hyposmia -----
  
  insert_msg('Figure 3: knetics of self-reported hyposmia')
  
  paper_figures$hyposmia_kinetic <- plot_grid(kinet$kinet_plots$survey$survey_at$anosmia + 
                                                labs(title = 'HACT: Austria, hypo/anosmia') + 
                                                theme(legend.position = 'none') + 
                                                scale_y_continuous(limits = c(0, 80)), 
                                              kinet$kinet_plots$survey$survey_it$anosmia + 
                                                labs(title = 'HACT: Italy, hypo/anosmia') + 
                                                theme(legend.position = 'none') + 
                                                scale_y_continuous(limits = c(0, 80)),
                                              get_legend(kinet$kinet_plots$survey$survey_at$anosmia), 
                                              kinet$kinet_plots$covild$A$anosmia_sympt + 
                                                labs(title = 'CovILD: ambulatory, hypo/anosmia') + 
                                                theme(legend.position = 'none') + 
                                                scale_y_continuous(limits = c(0, 50)), 
                                              kinet$kinet_plots$covild$HM$anosmia_sympt + 
                                                labs(title = 'CovILD: moderate, hypo/anosmia') + 
                                                theme(legend.position = 'none') + 
                                                scale_y_continuous(limits = c(0, 50)), 
                                              kinet$kinet_plots$covild$HS$anosmia_sympt + 
                                                labs(title = 'CovILD: severe, hypo/anosmia') + 
                                                theme(legend.position = 'none')  + 
                                                scale_y_continuous(limits = c(0, 50)), 
                                              ncol = 3, 
                                              align = 'hv', 
                                              axis = 'tblr', 
                                              labels = c('A', '', '', 'B'), 
                                              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_3_hyposmia_kinetics', 
                     w = 180, 
                     h = 150)
  
# Figure 4: Co-occurrence of self-reported hypo/anosmia and other symptoms ------
  
  insert_msg('Figure 4: co-occurence of hypo/anosmia and other symptoms')
  
  paper_figures$hyposmia_overlap <- plot_grid(plot_grid(hypo_overlap$radial_plots$survey$survey_at + 
                                                          labs(title = 'HACT: Austria') + 
                                                          theme(legend.position = 'none', 
                                                                plot.subtitle = element_blank()), 
                                                        hypo_overlap$radial_plots$survey$survey_it  +
                                                          labs(title = 'HACT: Italy') + 
                                                          theme(legend.position = 'none', 
                                                                plot.subtitle = element_blank()), 
                                                        nrow = 2, 
                                                        align = 'hv'), 
                                              plot_grid(hypo_overlap$radial_plots$covild$A + 
                                                          labs(title = 'CovILD: ambulatory') + 
                                                          theme(legend.position = 'none', 
                                                                plot.subtitle = element_blank()), 
                                                        hypo_overlap$radial_plots$covild$HM + 
                                                          labs(title = 'CovILD: moderate') + 
                                                          theme(legend.position = 'none', 
                                                                plot.subtitle = element_blank()), 
                                                        hypo_overlap$radial_plots$covild$HS + 
                                                          labs(title = 'CovILD: severe') + 
                                                          theme(legend.position = 'none', 
                                                                plot.subtitle = element_blank()), 
                                                        nrow = 3, 
                                                        align = 'hv') %>% 
                                                plot_grid(get_legend(hypo_overlap$radial_plots$covild$A + 
                                                                       theme(legend.position = 'right')), 
                                                          ncol = 2, 
                                                          rel_widths = c(0.85, 0.15)), 
                                              nrow = 2, 
                                              rel_heights = c(2, 3), 
                                              labels = LETTERS, 
                                              label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_4_hyposmia_cooccurrence', 
                     w = 180, 
                     h = 230)
  
# Figure 5: Definition of chronic COVID subsets ------  
  
  insert_msg('Figure 5: definition of the chronic COVID-19 phenotypes')
  
  paper_figures$clustering$top_panel <- plot_grid(pheno$heat_maps$survey_at + 
                                                    theme(plot.subtitle = element_blank(), 
                                                          legend.position = 'none'), 
                                                  pheno$heat_maps$survey_it + 
                                                    theme(plot.subtitle = element_blank(), 
                                                          legend.position = 'none', 
                                                          axis.text.y = element_blank()), 
                                                  ncol = 2, 
                                                  rel_widths = c(1.15, 0.85)) %>% 
    plot_grid(get_legend(pheno$heat_maps$survey_at), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1))
  
  paper_figures$clustering$bottom_panel <- list(pheno$clust_ft$plots$survey_at$anosmia, 
                                                pheno$clust_ft$plots$survey_it$anosmia, 
                                                pheno$clust_ft$plots$survey_at$taste_loss, 
                                                pheno$clust_ft$plots$survey_it$taste_loss, 
                                                pheno$clust_ft$plots$survey_at$fatigue_day, 
                                                pheno$clust_ft$plots$survey_it$fatigue_day) %>% 
    map(function(x) x + theme(plot.tag = element_blank(), 
                              legend.position = 'none')) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv') %>% 
    plot_grid(get_legend(pheno$heat_maps$survey_at), 
              ncol = 2, 
              rel_widths = c(0.9, 0.1))
  
  paper_figures$clustering <- plot_grid(paper_figures$clustering$top_panel, 
                                        paper_figures$clustering$bottom_panel, 
                                        nrow = 2, 
                                        rel_heights = c(1.2, 2.8), 
                                        labels = LETTERS, 
                                        label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_5_chronic_phenotypes', 
                     w = 180, 
                     h = 230)
  
# Figure 6: 100-day hyposmia risk modeling -----
  
  insert_msg('Figure 6: 100 day hyposmia risk modeling')
  
  paper_figures$hyposmia_risk <- plot_grid(uni_model$forest_plots$covild$self + 
                                             theme(legend.position = 'none', 
                                                   plot.subtitle = element_blank()), 
                                           get_legend(uni_model$forest_plots$survey$survey_at), 
                                           uni_model$forest_plots$survey$survey_at + 
                                             theme(legend.position = 'none', 
                                                   plot.subtitle = element_blank()) + 
                                             scale_x_continuous(limits = c(0.02, 32), 
                                                                trans = 'log2'), 
                                           uni_model$forest_plots$survey$survey_it + 
                                             theme(legend.position = 'none', 
                                                   plot.subtitle = element_blank()) + 
                                             scale_x_continuous(limits = c(0.02, 32),
                                                                trans = 'log2'), 
                                           ncol = 2, 
                                           rel_heights = c(1, 1.5), 
                                           align = 'hv', 
                                           axis = 'tblr', 
                                           labels = c('A', '', 'B'), 
                                           label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_6_hyposmia_risk_factors', 
                     w = 180, 
                     h = 180)
  
# Supplementary Figure S1: mean/median time to symptom recovery for particular features ----  
  
  insert_msg('Figure S1: median time to recovery')
  
  suppl_figures$kinetic_beta <- plot_grid(kinet$beta_summ_plots$survey$survey_at + 
                                            theme(legend.position = 'none', 
                                                  plot.subtitle = element_blank(), 
                                                  axis.text.x = element_text(angle = 45, hjust = 1)) + 
                                            labs(title = 'HACT: Austria'), 
                                          kinet$beta_summ_plots$survey$survey_it + 
                                            theme(legend.position = 'none', 
                                                  plot.subtitle = element_blank(), 
                                                  axis.text.x = element_text(angle = 45, hjust = 1)) +
                                            labs(title = 'HACT: Italy')) %>% 
    as_figure_object(figure_label = 'figure_s1_kinetic_modeling_beta',
                     w = 180, 
                     h = 140)
  
# Supplementary Figure S2: COVID-19 symptoms with low chronicity potential -----
  
  insert_msg('Figure S2: low chronicity potential')
  
  suppl_figures$low_chronicity <- list(kinet$kinet_plots$survey$survey_at$ague, 
                                       kinet$kinet_plots$survey$survey_it$ague, 
                                       kinet$kinet_plots$survey$survey_at$fever, 
                                       kinet$kinet_plots$survey$survey_it$fever) %>% 
    map(function(x) x + 
          theme(legend.position = 'none') + 
          scale_y_continuous(limits = c(0, 100))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = c('A', '', 'B'), 
              label_size = 10) %>% 
    plot_grid(get_legend(kinet$kinet_plots$survey$survey_at$ague + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure_object(figure_label = 'figure_s2_low_chronicity', 
                     w = 180, 
                     h = 145)
  
# Supplementary Figure S3: COVID-19 symptoms with high chronicity potential ----
  
  insert_msg('Figure S3: high chronicity potential')
  
  suppl_figures$high_chronicity <- list(kinet$kinet_plots$survey$survey_at$fatigue, 
                                       kinet$kinet_plots$survey$survey_it$fatigue, 
                                       kinet$kinet_plots$survey$survey_at$fatigue_day, 
                                       kinet$kinet_plots$survey$survey_it$fatigue_day, 
                                       kinet$kinet_plots$survey$survey_at$taste_loss, 
                                       kinet$kinet_plots$survey$survey_it$taste_loss) %>% 
    map(function(x) x + 
          theme(legend.position = 'none') + 
          scale_y_continuous(limits = c(0, 100))) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv', 
              labels = c('A', '', 'B', '', 'C'), 
              label_size = 10) %>% 
    plot_grid(get_legend(kinet$kinet_plots$survey$survey_at$ague + 
                           theme(legend.position = 'bottom')), 
              nrow = 2, 
              rel_heights = c(0.9, 0.1)) %>% 
    as_figure_object(figure_label = 'figure_s3_high_chronicity', 
                     w = 180, 
                     h = 220)
  
# Supplementary Figure S4: overlap between the sniffing test results and self-declared hypo/anosmia -----
  
  insert_msg('Figure S4: Overlap between the self-declared and test hyposmia')
  
  suppl_figures$hypo_sniffing <- plot_grid(plot_grid(plotlist =  hypo_kappa$cross_tbl_plots$hyposmia_mod_severe_V2[c('A', 'HM', 'HS')] %>% 
                                                       map(function(x) x + 
                                                             theme(legend.position = 'none', 
                                                                   plot.subtitle = element_blank())), 
                                                     ncol = 3), 
                                           plot_grid(plotlist =  hypo_kappa$cross_tbl_plots$hyposmia_severe[c('A', 'HM', 'HS')] %>% 
                                                       map(function(x) x + 
                                                             theme(legend.position = 'none', 
                                                                   plot.subtitle = element_blank())), 
                                                     ncol = 3), 
                                           get_legend(hypo_kappa$cross_tbl_plots$hyposmia_mod_severe_V2$A + 
                                                        theme(legend.position = 'bottom')), 
                                           hypo_kappa$plot + 
                                             theme(plot.tag.position = 'right'), 
                                           nrow = 4,
                                           rel_heights = c(0.9, 0.9, 0.2, 1.1), 
                                           labels = c('A', 'B', '', 'C'), 
                                           label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s4_sniffing_test', 
                     w = 180, 
                     h = 190)
  
# Supplementary Figure S5: 5-nearest neighbors of hyposmia ------
  
  insert_msg('Figure S5: 5 nearest co-occuring symptoms')
  
  suppl_figures$hyposmia_knn <- plot_grid(plot_grid(hypo_overlap$knn_plots$survey$survey_at + 
                                                      labs(title = 'HACT: Austria') + 
                                                      theme(legend.position = 'none', 
                                                            plot.subtitle = element_blank()), 
                                                    hypo_overlap$knn_plots$survey$survey_it + 
                                                      labs(title = 'HACT: Italy') + 
                                                      theme(legend.position = 'none', 
                                                            plot.subtitle = element_blank()), 
                                                    nrow = 2, 
                                                    align = 'hv'), 
                                          plot_grid(hypo_overlap$knn_plots$covild$A + 
                                                      labs(title = 'CovILD: ambulatory') + 
                                                      theme(legend.position = 'none', 
                                                            plot.subtitle = element_blank()), 
                                                    hypo_overlap$knn_plots$covild$HM + 
                                                      labs(title = 'CovILD: moderate') + 
                                                      theme(legend.position = 'none', 
                                                            plot.subtitle = element_blank()), 
                                                    hypo_overlap$knn_plots$covild$HS + 
                                                      labs(title = 'CovILD: severe') + 
                                                      theme(legend.position = 'none', 
                                                            plot.subtitle = element_blank()), 
                                                    nrow = 3, 
                                                    align = 'hv') %>% 
                                            plot_grid(get_legend(hypo_overlap$knn_plots$covild$A + 
                                                                   theme(legend.position = 'right')), 
                                                      ncol = 2,
                                                      rel_widths = c(0.8, 0.2)), 
                                          nrow = 2, 
                                          rel_heights = c(2, 3), 
                                          labels = LETTERS, 
                                          label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s5_hyposmia_nearest_neighbors', 
                     w = 180, 
                     h = 230)

# Supplementary Figure S6: cluster diagnostic plots ------
  
  insert_msg('Clustering diagnostic plots')
  
  suppl_figures$clustering <- plot_grid(plot_train_som(pheno$clust_lst$survey_at$kohonen_obj) + 
                                          globals$common_theme + 
                                          theme(plot.tag = element_blank()) +  
                                          labs(title = 'HACT: Austria'), 
                                        plot_train_som(pheno$clust_lst$survey_it$kohonen_obj) + 
                                          globals$common_theme + 
                                          theme(plot.tag = element_blank()) +  
                                          labs(title = 'HACT: Italy'), 
                                        pheno$clust_lst$survey_at$clust_obj$diagnostic_plots$wss + 
                                          labs(title = 'HACT: Austria') + 
                                          globals$common_theme + 
                                          theme(plot.subtitle = element_blank()), 
                                        pheno$clust_lst$survey_it$clust_obj$diagnostic_plots$wss + 
                                          labs(title = 'HACT: Italy') + 
                                          globals$common_theme + 
                                          theme(plot.subtitle = element_blank()), 
                                        pheno$clust_lst$survey_at$clust_obj$diagnostic_plots$dendrogram + 
                                          labs(title = 'HACT: Austria', 
                                               y = 'Manhattan distance') + 
                                          globals$common_theme + 
                                          theme(axis.line.x = element_blank(), 
                                                axis.title.x = element_blank(), 
                                                axis.text.x = element_blank(), 
                                                axis.ticks.x = element_blank()), 
                                        pheno$clust_lst$survey_it$clust_obj$diagnostic_plots$dendrogram + 
                                          labs(title = 'HACT: Italy', 
                                               y = 'Manhattan distance') + 
                                          globals$common_theme + 
                                          theme(axis.line.x = element_blank(), 
                                                axis.title.x = element_blank(), 
                                                axis.text.x = element_blank(), 
                                                axis.ticks.x = element_blank()), 
                                        ncol = 2, 
                                        align = 'hv', 
                                        axis = 'tblr', 
                                        labels = c('A', '', 'B', '', 'C'), 
                                        label_size = 10) %>% 
    as_figure_object(figure_label = 'figure_s6_clustering_qc', 
                     w = 180, 
                     h = 210)
  
  
# Saving the figures on the disc -----
  
  insert_msg('Saving the figures on the disc')
  
  paper_figures %>% 
    walk(save_figure_object, 
         target_folder = './paper/figures', 
         device = cairo_pdf)
  
  suppl_figures %>% 
    walk(save_figure_object, 
         target_folder = './paper/supplementary figures', 
         device = cairo_pdf)
  
# END -----
  
  insert_tail()