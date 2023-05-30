# Sample size required for clustering. 
# Clustering tendency of the HACT cohorts and clustering tendency of random 
# draws from the pooled dataset with different sizes, n

  insert_head()
  
# container -----
  
  eda_size <- list()

# Parallel backend -----
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# analysis datasets ---------
  
  insert_msg('Analysis datasets')
  
  ## datasets for the cohorts
  
  eda_size$cohort_tbl <- rec_time[c('north', 'south')] %>% 
    map(~filter(.x, complete.cases(.x))) %>% 
    map(column_to_rownames, 'ID')
  
  ## the pooled dataset
  
  eda_size$pooled_tbl <- eda_size$cohort_tbl %>% 
    reduce(rbind)

  ## size of the draws
  
  eda_size$draw_size <- c(50, seq(100, 900, by = 100))
  
  eda_size$draw_size <- 
    set_names(eda_size$draw_size, 
              paste0('n_', eda_size$draw_size))
  
  ## random draws IDs, 20 draws per size
  
  set.seed(1234)
  
  eda_size$draw_id <- eda_size$draw_size %>% 
    map(function(x) set_names(1:20, paste0('rep_', 1:20)) %>% 
          map(~sample(rownames(eda_size$pooled_tbl), 
                      size = x, 
                      replace = TRUE))) %>% 
    unlist(recursive = FALSE)
  
  ## draw lexicon
  
  eda_size$draw_lexicon <- 
    tibble(draw_id = names(eda_size$draw_id)) %>% 
    mutate(repetition = stri_extract(draw_id, regex = 'rep_\\d+$'), 
           n = stri_extract(draw_id, regex = '\\d{2,3}'), 
           n = as.numeric(n))
  
# Clustering tendencies of the cohort datasets ------
  
  insert_msg('Clustering tendencies of the cohort datasets')
  
  eda_size$tendency_cohorts <- eda_size$cohort_tbl %>% 
    future_map(~get_clust_tendency(data = .x, 
                                   n = floor(nrow(.x) * 0.5), 
                                   seed = 1234), 
               .options = furrr_options(seed = TRUE))
  
# Clustering tendencies of the draws -------
  
  insert_msg('Clustering tendencies of the draws')
  
  eda_size$draw_stats <- eda_size$draw_id %>% 
    future_map(draw_hopkins, 
               data = eda_size$pooled_tbl, 
               .options = furrr_options(seed = TRUE)) %>% 
    compress(names_to = 'draw_id') %>% 
    select(-n) %>% 
    left_join(eda_size$draw_lexicon, ., by = 'draw_id')
  
# Summary stats -----
  
  insert_msg('Summary stats')

  eda_size$summary_stats <- 
    eda_size$draw_stats %>% 
    group_by(n) %>% 
    summarise(median = median(hopkins_stat), 
              q25 = quantile(hopkins_stat, 0.25), 
              q75 = quantile(hopkins_stat, 0.75), 
              min = min(hopkins_stat), 
              max = max(hopkins_stat), 
              mean = mean(hopkins_stat), 
              sd = sd(hopkins_stat))
  
# Plotting -----
  
  insert_msg('Plotting')
  
  ## text to be displayed in the plot: n number and H values
  ## for the HACT cohorts
  
  eda_size$plot_txt <- 
    list(x = globals$hact_labs, 
         y = map_dbl(eda_size$cohort_tbl, nrow), 
         z = map_dbl(eda_size$tendency_cohorts, ~.x$hopkins_stat)) %>% 
    pmap(function(x, y, z) paste0(x, ', n = ', y, ', H = ', signif(z, 2)))

  ## plot
  
  eda_size$plot <- eda_size$draw_stats %>% 
    ggplot(aes(x = n, 
               y = hopkins_stat)) + 
    geom_boxplot(aes(x = n, 
                     y = hopkins_stat, 
                     group = n), 
                 alpha = 0.25, 
                 outlier.color = NA, 
                 fill = 'steelblue') + 
    geom_point(shape = 21, 
               size = 2, 
               alpha = 0.75, 
               fill = 'steelblue', 
               position = position_jitter(width = 4, height = 0)) + 
    geom_smooth() + 
    geom_vline(xintercept = nrow(eda_size$cohort_tbl$north), 
               linetype = 'dashed', 
               color = globals$hact_colors['north']) + 
    geom_vline(xintercept = nrow(eda_size$cohort_tbl$south), 
               linetype = 'dashed', 
               color = globals$hact_colors['south']) + 
    expand_limits(y = 0.5) + 
    scale_x_continuous(breaks = seq(100, 900, by = 100)) + 
    globals$common_theme + 
    labs(title = 'Sample size and clustering tendency', 
         subtitle = ('20 random draws from the pooled AT/IT dataset per sample size'), 
         x = 'sample size, n', 
         y = 'Hopkins statistic') + 
    annotate('text', 
             label = eda_size$plot_txt$north, 
             size = 2.75, 
             color = globals$hact_colors['north'],
             x = 500, 
             y = 0.6, 
             vjust = 0, 
             hjust = 0) +
    annotate('text', 
             label = eda_size$plot_txt$south, 
             size = 2.75, 
             color = globals$hact_colors['south'], 
             x = 400,
             y = 0.6, 
             hjust = 1, 
             vjust = 0)
    
  
# Caching the results -------
  
  insert_msg('Caching the results')
  
  save(eda_size, file = './cache/eda_size.RData')

# END ------
  
  plan('sequential')
  
  insert_tail()