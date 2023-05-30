# Importance of the class defining factors

  insert_head()
  
# container -------
  
  clust_imp <- list()
  
# Parallel backend ------
  
  insert_msg('Parallel backend')

  plan('multisession')
  
# Variable importance -----
  
  insert_msg('Variable importance')
  
  ## 20 runs
  
  set.seed(1234)
  
  clust_imp$importance <- sample(1:1000, 20, replace = FALSE) %>% 
    set_names(paste0('run_', 1:20)) %>% 
    future_map(function(x) impact(part_clust$clust_north, 
                                  seed = x, 
                                  .parallel = FALSE), 
               .options = furrr_options(seed = TRUE))
  
# Importance stats -------
  
  insert_msg('Stats')
  
  clust_imp$stats <- clust_imp$importance %>% 
    map(filter, variable != 'data') %>% 
    compress(names_to = 'run')
  
# Plotting -------
  
  insert_msg('Plotting')
  
  clust_imp$plot <- clust_imp$stats %>% 
    ggplot(aes(x = frac_diff, 
               y = reorder(variable, frac_diff))) + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    geom_boxplot(outlier.color = NA, 
                 fill = globals$hact_colors[1], 
                 alpha = 0.5) + 
    #geom_point(shape = 16, 
     #          size = 2, 
      #         alpha = 0.5, 
       #        color = 'black', 
        #       position = position_jitter(width = 00, height = 0.1)) + 
    scale_y_discrete(labels = function(x) exchange(x, dict = hact$dict)) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Importance of clustering variables', 
         subtitle = 'AT, survey study', 
         x = expression(Delta * 'fraction explained variance'))
  
# caching ------
  
  insert_msg('Saving the results')
  
  save(clust_imp, file = './cache/clust_imp.RData')
  
# END ------
  
  plan('sequential')
  
  insert_tail()
