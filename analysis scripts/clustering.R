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
                                    kNN = 7, 
                                    simple_vote = FALSE, 
                                    resolve_ties = TRUE)
  
  part_clust$clust_south$clust_assignment <- 
    part_clust$clust_south$clust_assignment %>% 
    mutate(clust_id = factor(clust_id, c('#1', '#2', '#3'))) 
  
# Cluster n numbers -------
  
  insert_msg('Cluster N numbers')
  
  part_clust$n_numbers <- part_clust[c("clust_north", "clust_south")] %>% 
    map(ngroups) %>% 
    map(arrange, desc(clust_id)) %>% 
    map(mutate, 
        percent = n/sum(n) * 100,
        x_pos = cumsum(percent) - 0.5 * percent)
  
  part_clust$n_labs <- part_clust$n_numbers %>% 
    map(~map2_chr(.x[[1]], .x[[2]], 
                  paste, sep = '\nn = ')) %>% 
    map2(., part_clust$n_numbers, 
         ~set_names(.x, .y[[1]]))
  
  ## plotting the cluster distribution
  
  part_clust$n_plot <- part_clust$n_numbers %>% 
    compress(names_to = 'cohort') %>% 
    ggplot(aes(x = percent, 
               y = cohort, 
               fill = clust_id)) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = 'stack') + 
    geom_label(aes(label = signif(percent, 2), 
                   x = x_pos), 
               size = 2.75, 
               show.legend = FALSE) + 
    scale_fill_manual(values = globals$clust_colors, 
                      name = 'Cluster') + 
    scale_y_discrete(labels = c(clust_north = paste('AT\nn =', 
                                                    nrow(part_clust$analysis_tbl$north)), 
                                clust_south = paste('IT\nn =', 
                                                    nrow(part_clust$analysis_tbl$south)))) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) +
    labs(title = 'Cluster distribution, survey study', 
         x = '% of cohort')
  
# Clustering variances ------
  
  insert_msg('Clustering variances')
  
  ## fractions of explained variances
  
  part_clust$variance <- part_clust[c("clust_north", "clust_south")] %>% 
    map(var) %>% 
    map_dbl(~.x$frac_var) %>% 
    compress(names_to = 'cohort', 
             values_to = 'frac_var') %>% 
    mutate(cohort = stri_extract(cohort, regex = 'south|north'), 
           cohort = factor(cohort, c('south', 'north')))
  
  ## plots
  
  part_clust$variance_plot <- part_clust$variance %>% 
    ggplot(aes(x = frac_var, 
               y = cohort, 
               fill = cohort)) + 
    geom_bar(stat = 'identity', 
             color = 'black') + 
    geom_text(aes(label = signif(frac_var, 2)), 
              size = 2.75, 
              color = 'white', 
              hjust = 1.4) + 
    scale_fill_manual(values = globals$hact_colors, 
                      labels = globals$hact_labs) + 
    scale_y_discrete(labels = globals$hact_labs) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Explained clustering variance', 
         x = 'Fraction of explained clustering variance')
  
# Diagnostic of the clustering objects ------
  
  insert_msg('Characteristic of the clustering objects')
  
  ## diagnostic plots
  
  part_clust$diagnostic_plots <- part_clust$clust_north %>% 
    plot(type = 'diagnostic', 
         cust_theme = globals$common_theme)
  
  ## distance heat maps
  
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
        cust_theme = globals$common_theme)
  
  ## UMAP plots
  
  part_clust$umap_plots <- part_clust[c('clust_north', 
                                        'clust_south')] %>% 
    map(plot, 
        type = 'components', 
        red_fun = 'umap', 
        with = 'data', 
        kdim = 2, 
        cust_theme = globals$common_theme)
  
  ## MDS plots of the distance matrices
  
  part_clust$mds_plots <- part_clust[c('clust_north', 
                                       'clust_south')] %>% 
    map(plot, 
        type = 'components', 
        red_fun = 'mds', 
        with = 'distance', 
        kdim = 2, 
        cust_theme = globals$common_theme)
  
  ## adjustment 
  
  for(i in c('pca_plots', 'umap_plots', 'mds_plots')) {
    
    part_clust[[i]] <- 
      list(x = part_clust[[i]], 
           y = c('AT, survey study', 'IT, survey study'), 
           z = part_clust$n_labs) %>% 
      pmap(function(x, y, z) x + 
             scale_fill_manual(values = globals$clust_colors, 
                               labels = z) + 
             labs(subtitle = y) + 
             theme(plot.tag = element_blank()))
    
  }
  
# Feature heat map -----
  
  insert_msg('Clustering feature heat maps')
  
  part_clust$feature_hm <- list(x_object = part_clust[c('clust_north', 
                                                        'clust_south')], 
                                plot_subtitle = c('AT, survey study', 
                                                  'IT, survey study')) %>% 
    pmap(plot_clust_hm, 
         plot_title = 'Recovery time', 
         x_lab = 'Participant', 
         fill_lab = 'Recovery\ndays post CoV', 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          scale_y_discrete(labels = function(x) exchange(x, dict = hact$dict), 
                           limits = globals$hact_symptom_order) + 
          labs(tag = paste0('\n', .x$labels$tag)))
  
# END ----
  
  rm(i)
  
  insert_tail()