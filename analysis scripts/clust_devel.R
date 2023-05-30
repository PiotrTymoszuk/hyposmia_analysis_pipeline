# Choice of the best clustering algorithm for the participant clustering 
# by the symptom-specific recovery time. The clustering is done in the AT cohort
# of the HACT study. Pre-processing: mean-centered normalization.

  insert_head()
  
# container list -----
  
  cl_devel <- list()
  
# globals: analysis tables -----
  
  insert_msg('Analysis tables')
  
  cl_devel$analysis_tbl <- rec_time[c('north', 'south')] %>% 
    map(~filter(.x, complete.cases(.x))) %>% 
    map(column_to_rownames, 'ID')
  
  cl_devel$test_dist <- c('euclidean', 'manhattan', 'sumofsquares', 'cosine')
  
  cl_devel$som_x <- floor(sqrt(5 * sqrt(nrow(cl_devel$analysis_tbl$north))))
  
# generating clust_analysis objects ----
  
  insert_msg('Creating the clustering objects')
  
  ## hierarchical clustering
  
  plan('multisession')
  
  cl_devel$algos[paste0('hcl_', cl_devel$test_dist)] <- 
    cl_devel$test_dist %>% 
    future_map(~hcluster(data = cl_devel$analysis_tbl$north, 
                  distance_method = .x, 
                  hc_method = 'ward.D2', 
                  k = 3, 
                  seed = 1234), 
               .options = furrr_options(seed = TRUE))
  
  ## K-Means
  
  cl_devel$algos[paste0('kmeans_', cl_devel$test_dist)] <- 
    cl_devel$test_dist %>% 
    future_map(~kcluster(data = cl_devel$analysis_tbl$north, 
                         distance_method = .x, 
                         clust_fun = 'kmeans', 
                         k = 3, 
                         seed = 1234), 
               .options = furrr_options(seed = TRUE))
  
  ## PAM
  
  cl_devel$algos[paste0('pam_', cl_devel$test_dist)] <- 
    cl_devel$test_dist %>% 
    future_map(~kcluster(data = cl_devel$analysis_tbl$north, 
                         distance_method = .x, 
                         clust_fun = 'pam', 
                         k = 3, 
                         seed = 1234),
               .options = furrr_options(seed = TRUE))
  
  ## combi SOM + HCl clustering
  
  cl_devel$algos[paste0('combi_', cl_devel$test_dist)] <- 
    cl_devel$test_dist %>% 
    future_map(~combi_cluster(data = cl_devel$analysis_tbl$north, 
                              distance_som = .x, 
                              xdim = cl_devel$som_x, 
                              ydim = cl_devel$som_x, 
                              topo = 'hexagonal', 
                              neighbourhood.fct = 'gaussian', 
                              toroidal = FALSE, 
                              rlen = 1500, 
                              node_clust_fun = hcluster, 
                              distance_nodes = 'euclidean', 
                              k = 3, 
                              seed = 1234), 
               .options = furrr_options(seed = TRUE, 
                                        packages = c('clustTools', 
                                                     'somKernels')))

  plan('sequential')
  
# Calculating the clustering variances ----
  
  insert_msg('Clustering variances')
  
  cl_devel$variances <- cl_devel$algos %>% 
    map(var) %>% 
    map_dbl(~.x$frac_var) %>% 
    compress(names_to = 'method', 
             values_to = 'frac_var')
  
# Cross-validation -----
  
  insert_msg('Cross-validation')
  
  cl_devel$cv_objects <- cl_devel$algos %>% 
    map(cv, 
        nfolds = 10, 
        kNN = 7, 
        resolve_ties = TRUE, 
        simple_vote = FALSE, 
        seed = 1234, 
        .parallel = TRUE)
  
  cl_devel$cv_results <- cl_devel$cv_objects %>% 
    map(~.x$summary) %>% 
    compress(names_to = 'method')
  
  cl_devel$test_results <- 
    left_join(cl_devel$variances, 
              cl_devel$cv_results, 
              by = 'method') %>% 
    mutate(cv_accuracy = 1 - mean_error)
  
# Plotting the variance and the Cv correct rate for the algorithms ----
  
  insert_msg('Variance and CV accuracy plot')
  
  cl_devel$result_plot <-  cl_devel$test_results %>% 
    select(method, frac_var, cv_accuracy) %>% 
    pivot_longer(cols = c('frac_var', 'cv_accuracy'), 
                 names_to = 'statistic', 
                 values_to = 'value') %>% 
    mutate(method = stri_replace(method, 
                                 fixed = '_',
                                 replacement = ', '), 
           method = stri_replace(method, 
                                 fixed = 'combi', 
                                 replacement = 'SOM + HCl'), 
           method = stri_replace(method, 
                                 fixed = 'hcl', 
                                 replacement = 'HCl'), 
           method = stri_replace(method, 
                                 fixed = 'kmeans', 
                                 replacement = 'k-means'), 
           method = stri_replace(method, fixed = 'pam', 
                                 replacement = 'PAM'))
  
  
  cl_devel$result_plot <- cl_devel$result_plot %>% 
    ggplot(aes(x = value, 
               y = reorder(method, value), 
               fill = statistic)) + 
    geom_bar(stat = 'identity', 
             color = 'black', 
             position = position_dodge(0.9)) + 
    scale_fill_manual(values = c(frac_var = 'steelblue', 
                                 cv_accuracy = 'coral3'), 
                      labels = c(frac_var = 'Clust. variance', 
                                 cv_accuracy = 'CV accuracy'), 
                      name = '') + 
    globals$common_theme + 
    theme(axis.title.y = element_blank()) + 
    labs(title = 'Performance of clustering algorithms', 
         subtitle = 'training AT, survey study', 
         x = 'Statistic value')
  
# saving the results ------
  
  insert_msg('Caching the results')
  
  save(cl_devel, file = './cache/cl_devel.RData')
  
# END -----
  
  insert_tail()