# Calculates distances (SMC) between particular symptoms for 
# subsequent time points. Performs MDS to assess the symptom overlap.
# The relation with the most overlapping symptoms is analyzed by comparing
# the mean 5-NN SMC distances between the symptoms.

  insert_head()
  
# container list -----
  
  sympt_dist <- list()
  
# globals -----
  
  insert_msg('Globals')
  
  ## analysis tables
  
  sympt_dist$hact_tbl <- mod_tbl[c('north', 'south')] %>% 
    map(filter, time %in% c(3, 14, 28, 90)) %>%
    map(blast, time) %>% 
    map(map, select, -time) %>% 
    map(set_names, c('acute', 'sub_acute', 'long', 'pasc')) %>% 
    unlist(recursive = FALSE) %>% 
    map(~filter(.x, complete.cases(.x))) %>% 
    map(column_to_rownames, 'ID') %>% 
    map(t)
  
  sympt_dist$covild_tbl <- mod_tbl$covild %>% 
    blast(cat_WHO, time_numeric) %>% 
    map(select, -time_numeric, -cat_WHO) %>% 
    map(~filter(.x, complete.cases(.x))) %>% 
    map(~map_dfr(.x, function(x) if(is.factor(x)) as.numeric(x) - 1 else x)) %>% 
    map(column_to_rownames, 'ID') %>% 
    map(t)
  
  ## n numbers
  
  sympt_dist$n_numbers_hact <- sympt_dist$hact_tbl[c(1, 5)] %>% 
    map_dbl(ncol) %>% 
    set_names(c('north', 'south'))
  
  sympt_dist$n_numbers_covild <- sympt_dist$covild_tbl[c(1, 6, 11)] %>% 
    map_dbl(ncol) %>% 
    set_names(c('A', 'HM', 'HS'))
  
# Matrices with the SMC distances -----
  
  insert_msg('Matrices with the SMC distances')
  
  sympt_dist[c("dist_hact", "dist_covild")] <- 
    sympt_dist[c("hact_tbl", "covild_tbl")] %>% 
    map(map, calculate_dist, 'smc')

# Multi-dimensional scaling -------
  
  insert_msg('MDS to visualize the distances')
  
  sympt_dist[c("mds_hact", "mds_covild")] <- 
    sympt_dist[c("hact_tbl", "covild_tbl")] %>% 
    map(map, 
        reduce_data, 
        distance_method = 'smc',
        kdim = 2, 
        red_fun = 'mds')
  
# Two-dimensional MDS plots, HACT ------
  
  insert_msg('Plotting the MDS results, HACT')
  
  sympt_dist$mds_plots_hact <- 
    list(x = sympt_dist$mds_hact, 
         point_color = c(rep(globals$hact_colors[1], 4), 
                         rep(globals$hact_colors[2], 4))) %>% 
    pmap(plot, 
         type = 'scores', 
         cust_theme = globals$common_theme)
  
  sympt_dist$mds_plots_hact <- 
    list(x = sympt_dist$mds_plots_hact, 
         y = rep(c('0 - 14 days', 
                   '14 days', 
                   '28 days', 
                   '3 months'), 2), 
         z = c(rep('AT, survey study', 4), 
               rep('IT, survey study', 4)), 
         v = paste('n =', map_dbl(sympt_dist$hact_tbl, ncol))) %>% 
    pmap(function(x, y, z, v) x + 
           labs(title = y, 
                subtitle = paste(z, v, sep = ', ')) + 
           geom_text_repel(aes(label = exchange(observation, 
                                                dict = hact$dict), 
                               color = ifelse(observation %in% c('anosmia', 'taste_loss'), 
                                              'high', 'low'), 
                               fontface = ifelse(observation %in% c('anosmia', 'taste_loss'), 
                                                 'bold', 'plain')), 
                           size = 2.75, 
                           max.overlaps = 7, 
                           force = 2, 
                           force_pull = 0.5) + 
           scale_color_manual(values = c(high = 'black', low = 'gray50')) + 
           guides(color = 'none') + 
           theme(plot.tag = element_blank()))
  
# Two-dimensional MDS plots, CovILD ------
  
  insert_msg('Plotting the MDS results, CovILD')
  
  sympt_dist$mds_plots_covild <- 
    list(x = sympt_dist$mds_covild, 
         point_color = c(rep(globals$covild_colors[1], 5), 
                         rep(globals$covild_colors[2], 5), 
                         rep(globals$covild_colors[3], 5))) %>% 
    pmap(plot, 
         type = 'scores', 
         cust_theme = globals$common_theme)
  
  sympt_dist$mds_plots_covild <- 
    list(x = sympt_dist$mds_plots_covild, 
         y = rep(c('Acute CoV', 
                   '2-month FUP', 
                   '3-month FUP', 
                   '6-month FUP', 
                   '1-year FUP'), 3), 
         z = c(rep('Severe, CovILD study', 5), 
               rep('Moderate, CovILD study', 5), 
               rep('Severe, CovILD study', 5)), 
         v = paste('n =', map_dbl(sympt_dist$covild_tbl, ncol))) %>% 
    pmap(function(x, y, z, v) x + 
           labs(title = y, 
                subtitle = paste(z, v, sep = ', ')) + 
           geom_text_repel(aes(label = exchange(observation, 
                                                dict = covild$dict)), 
                           size = 2.75) + 
           theme(plot.tag = element_blank()))
  
# mean 5-NN, HACT -------
  
  insert_msg('kNN distances, HACT')

  sympt_dist$mean_kNN_hact <- sympt_dist$dist_hact %>% 
    map(~kNNdist(as.dist(.x), k = 5, all = TRUE)) %>% 
    map(function(mtx) rownames(mtx) %>% 
          map(~mean(mtx[.x, ])) %>% 
          set_names(rownames(mtx))) %>% 
    map(~map2_dfr(.x, names(.x), ~tibble(variable = .y, mean_dist = .x)))
  
  sympt_dist$mean_kNN_hact <- 
    list(north = sympt_dist$mean_kNN_hact[1:4], 
         south = sympt_dist$mean_kNN_hact[5:8]) %>% 
    map(~map2_dfr(., c(3, 14, 28, 90), ~mutate(.x, time = .y))) %>% 
    map(mutate, symptom = exchange(variable, dict = hact$dict))

# mean 5-NN distances, CovILD -------
  
  insert_msg('kNN distances, CovILD')
  
  ## HACT, 5-NN
  
  sympt_dist$mean_kNN_covild <- sympt_dist$dist_covild %>% 
    map(~kNNdist(as.dist(.x), k = 5, all = TRUE)) %>% 
    map(function(mtx) rownames(mtx) %>% 
          map(~mean(mtx[.x, ])) %>% 
          set_names(rownames(mtx))) %>% 
    map(~map2_dfr(.x, names(.x), ~tibble(variable = .y, mean_dist = .x)))
  
  sympt_dist$mean_kNN_covild <- 
    list(ambulatory = sympt_dist$mean_kNN_covild[1:5], 
         moderate = sympt_dist$mean_kNN_covild[6:10], 
         severe = sympt_dist$mean_kNN_covild[11:15]) %>% 
    map(~map2_dfr(., c(0, 60, 100, 180, 360), ~mutate(.x, time = .y))) %>% 
    map(mutate, symptom = exchange(variable, dict = covild$dict))

# Displaying the kNN distance in the plots -----
  
  insert_msg('kNN visualization')
  
  ## HACT
  
  sympt_dist$kNN_plots_hact <- 
    list(data = sympt_dist$mean_kNN_hact, 
         highlight_color = globals$hact_colors, 
         plot_subtitle = c('AT, survey study', 
                           'IT, survey study'), 
         plot_tag = paste('\nn =', sympt_dist$n_numbers_hact)) %>% 
    pmap(draw_knn_course, 
         label_var = 'symptom', 
         plot_title = 'Isolated symptoms', 
         x_lab = 'Mean 5-NN simple matching distance', 
         highlight_sympt = 'anosmia') %>% 
    map(tag_to_sub)
  
  ## CovILD
  
  sympt_dist$kNN_plots_covild <- 
    list(data = sympt_dist$mean_kNN_covild, 
         highlight_color = globals$covild_colors[1:3], 
         plot_subtitle = c('Ambulatory, CovILD study', 
                           'Moderate, CovILD study', 
                           'Severe, CovILD study'), 
         plot_tag = paste('\nn =', sympt_dist$n_numbers_covild)) %>% 
    pmap(draw_knn_course, 
         label_var = 'symptom', 
         plot_title = 'Isolated symptoms', 
         x_lab = 'Mean 5-NN simple matching distance', 
         highlight_sympt = 'anosmia_sympt') %>% 
    map(tag_to_sub)
  
# END -----
  
  insert_tail()