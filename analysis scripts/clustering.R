# semi-supervised clustering of the HACT data sets by the symptom recovery times
# AT: training cohort (PAM + eucllidean as tested), IT: test cohort.
# kNN label propagation algorithm.

  insert_head()
  
# container list -----
  
  part_clust <- list()
  
# globals -----
  
  insert_msg('Globals setup')
  
  part_clust$analysis_tbl <- cl_devel$analysis_tbl

  part_clust$clust_north <- cl_devel$algos$pam_euclidean
  
  part_clust$clust_north$clust_assignment <- 
    part_clust$clust_north$clust_assignment %>% 
    mutate(clust_id = car::recode(clust_id, 
                                  "'1' = '#3'; '2' = '#2'; '3' = '#1'"), 
           clust_id = factor(clust_id, c('#1', '#2', '#3'))) 
  
# cluster projection -----
  
  insert_msg('Predicting the cluster assignment in the IT cohort')
  
  part_clust$clust_south <- predict(part_clust$clust_north, 
                                    newdata = part_clust$analysis_tbl$south, 
                                    type = 'propagation', 
                                    kNN = 5)
  
  part_clust$clust_south$clust_assignment <- 
    part_clust$clust_south$clust_assignment %>% 
    mutate(clust_id = factor(clust_id, c('#1', '#2', '#3'))) 
  
# Diagnostic of the clustering objects ------
  
  insert_msg('Characteristic of the clustering objects')
  
  ## diagnostic plots
  
  part_clust$diagnostic_plots <- part_clust$clust_north %>% 
    plot(type = 'diagnostic', 
         cust_theme = globals$common_theme)
  
  ## diastance heat maps
  
  part_clust$heat_maps <- part_clust[c('clust_north', 
                                       'clust_south')] %>% 
    map(plot, 
        type = 'heat_map', 
        cust_theme = globals$common_theme) %>% 
    map2(., c('AT, survey study', 'IT, survey study'), 
         ~.x + 
           labs(subtitle = paste(.y, .x$labels$subtitle, sep = ', ')) + 
           theme(axis.text.x = element_blank(), 
                 axis.text.y = element_blank(), 
                 axis.ticks = element_blank()))
  
  ## PCA score plots
  
  part_clust$pca_plots <- part_clust[c('clust_north', 
                                        'clust_south')] %>% 
    map(plot, 
        type = 'components', 
        red_fun = 'pca', 
        with = 'data', 
        kdim = 2, 
        cust_theme = globals$common_theme) %>% 
    map2(., c('AT, survey study', 'IT, survey study'), 
         ~.x + 
           scale_fill_manual(values = globals$clust_colors) + 
           labs(subtitle = paste(.y, .x$labels$subtitle, sep = ', ')))
  
  ## UMAP plots
  
  part_clust$umap_plots <- part_clust[c('clust_north', 
                                        'clust_south')] %>% 
    map(plot, 
        type = 'components', 
        red_fun = 'umap', 
        with = 'data', 
        kdim = 2, 
        cust_theme = globals$common_theme) %>% 
    map2(., c('AT, survey study', 'IT, survey study'), 
         ~.x + 
           scale_fill_manual(values = globals$clust_colors) + 
           labs(subtitle = paste(.y, .x$labels$subtitle, sep = ', ')))
  
  ## MDS plots of the distance matrices
  
  part_clust$mds_plots <- part_clust[c('clust_north', 
                                       'clust_south')] %>% 
    map(plot, 
        type = 'components', 
        red_fun = 'mds', 
        with = 'data', 
        kdim = 2, 
        cust_theme = globals$common_theme) %>% 
    map2(., c('AT, survey study', 'IT, survey study'), 
         ~.x + 
           scale_fill_manual(values = globals$clust_colors) + 
           labs(subtitle = paste(.y, .x$labels$subtitle, sep = ', ')))
  
# Variable importance -----
  
  insert_msg('Variable importance')
  
  part_clust$importance <- impact(part_clust$clust_north, 
                                  seed = 1234, 
                                  .parallel = TRUE)
  
  part_clust$imp_plot <- plot(part_clust$importance, 
                              type = 'bar', 
                              fill_color = globals$hact_colors[1], 
                              cust_theme = globals$common_theme, 
                              plot_title = 'Importance of clustering variables', 
                              plot_subtitle = 'AT, survey study', 
                              label = FALSE) + 
    scale_y_discrete(labels = translate_var(globals$hact_symptoms, 
                                            dict = hact$dict))
  
# Feature heat map -----
  
  insert_msg('Clustering feature heat maps')
  
  part_clust$feature_hm <- list(x_object = part_clust[c('clust_north', 
                                                        'clust_south')], 
                                plot_subtitle = c('AT, survey study', 
                                                  'IT, survey study')) %>% 
    pmap(plot_clust_hm, 
         plot_title = 'Recovery time', 
         x_lab = 'Participant', 
         fill_lab = 'Days post CoV', 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          scale_y_discrete(labels = translate_var(globals$hact_symptoms, 
                                                  dict = hact$dict), 
                           limits = globals$hact_symptom_order) + 
          labs(tag = paste0('\n', .x$labels$tag)))

# END ----
  
  insert_tail()