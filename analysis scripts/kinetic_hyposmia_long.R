# Kinetic modeling of the log-term hyposmia in the HACT survey.
# Participants with an observation time >= 180 days at the 6 month time-point
# are included

  insert_head()
  
# container -------
  
  hyp_long <- list()
  
# globals -----
  
  insert_msg('Globals')
  
  hyp_long$symptoms <- c(anosmia = 'anosmia', 
                         taste_loss = 'taste_loss')
  
  hyp_long$analysis_tbl <- hact[c('north', 'south')] %>% 
    map(filter, obs_time >= 180) %>% 
    map(select, ID, all_of(hyp_long$symptoms))
  
  
  hyp_long$analysis_tbl <-  hyp_long$analysis_tbl %>% 
    map(function(cohort) cohort %>% 
          dlply(.(ID), as_tibble) %>% 
          map(select, -ID) %>% 
          map(~map_dfc(.x, function(sympt) switch(as.character(sympt), 
                                                  absent = c(0, 0, 0, 0, 0, 0), 
                                                  `1 - 3 days` = c(1, 0, 0, 0, 0, 0), 
                                                  `up to 1 week` = c(1, 1, 0, 0, 0, 0), 
                                                  `up to 2 weeks` = c(1, 1, 1, 0, 0, 0), 
                                                  `up to 4 weeks` = c(1, 1, 1, 1, 0, 0), 
                                                  `up to 3 months` = c(1, 1, 1, 1, 1, 0), 
                                                  `up to 6 months` = c(1, 1, 1, 1, 1, 1), 
                                                  `over 6 months` = c(1, 1, 1, 1, 1, 1)))) %>% 
          map2_dfr(., names(.), ~mutate(.x, ID = .y, time = c(3, 7, 14, 28, 90, 180)))) %>% 
    map(as_tibble) %>% 
    map(complete_cases)

# building the mixed-effect linear models ------
  
  insert_msg('Generating the models')
  
  hyp_long$models$north <- hyp_long$symptoms %>% 
    map(~safely(model_kinetic)(data = hyp_long$analysis_tbl$north, 
                               response = .x, 
                               time = 'time', 
                               ID = 'ID', 
                               family = 'binomial', 
                               order = 2))
  
  hyp_long$models$south <- hyp_long$symptoms %>% 
    map(~safely(model_kinetic)(data = hyp_long$analysis_tbl$south, 
                               response = .x, 
                               time = 'time', 
                               ID = 'ID', 
                               family = 'binomial', 
                               order = 2))
  
  hyp_long$models <- hyp_long$models %>% 
    map(~map(.x, ~.x$result)) %>% 
    map(compact)
  
# LRT -----
  
  insert_msg('LRT')
  
  ## LRT
  
  hyp_long$lrt <- hyp_long$models %>% 
    map(lrt_list, .parallel = TRUE)
  
  ## plot captions
  
  hyp_long$lrt_summ <- hyp_long$lrt %>% 
    map(~map_dfr(.x, filter, order == 'global')) %>% 
    map(mutate, p_value = Pr..Chisq.) %>% 
    map(re_adjust) %>% 
    map(mutate, 
        eff_size = paste('\u03BB =', signif(lambda, 2)), 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting ------
  
  insert_msg('Plotting')
  
  hyp_long$plots <- list(mod = hyp_long$models, 
                         sub = c('AT, survey study', 'IT, survey study'), 
                         col = globals$hact_colors) %>% 
    pmap(function(mod, sub, col) list(x = mod, 
                                      plot_title = translate_var(names(mod), 
                                                                 dict = hact$dict)) %>% 
           pmap(plot, 
                type = 'frequency', 
                plot_subtitle = sub, 
                x_lab = 'Days post CoV', 
                cust_theme = globals$common_theme, 
                point_color = col,
                outcome_color = col, 
                label_size = 2.75) %>% 
           map(~.x + 
                 theme(legend.position = 'none') + 
                 scale_y_continuous(limits = c(0, 100)) + 
                 scale_x_continuous(limits = c(0, 185), 
                                    breaks = c(0, 14, 28, 90, 180))))
  
# END -----
  
  insert_tail()