# Characteristic of the study cohorts (HACT and CovILD): baseline, 
# clinical history, CoV course and recovery

  insert_head()
  
# container list ------
  
  eda_cohort <- list()

# globals ------
  
  insert_msg('Globals setup')
  
  ## analysis table for covild
  
  eda_cohort$hact_tbl <- hact[c('north', 'south')] %>% 
    reduce(rbind) %>% 
    mutate(cohort = car::recode(cohort, 
                                "'north' = 'AT'; 'south' = 'IT'"), 
           cohort = factor(cohort, c('AT', 'IT')))
  
  ## test variables for HACT
  
  eda_cohort$hact_vars$variable <- globals[c('demo_vars', 
                                             'clinic_vars', 
                                             'cov_vars', 
                                             'psych_var')] %>% 
    reduce(c)
  
  eda_cohort$hact_vars$format <- 
    hact$north[eda_cohort$hact_vars$variable] %>% 
    map_lgl(is.numeric) %>% 
    ifelse('numeric', 'factor')

  ## test variables for CovILD
  
  eda_cohort$covild_vars$variable <- globals$covild_vars
  
  eda_cohort$covild_vars$format <- 
    covild$data[eda_cohort$covild_vars$variable] %>% 
    map_lgl(is.numeric) %>% 
    ifelse('numeric', 'factor')
  
  ## test and plot type
  
  eda_cohort[c("hact_vars", "covild_vars")] <- 
    eda_cohort[c("hact_vars", "covild_vars")] %>% 
    map(as_tibble) %>% 
    map2(., list('wilcoxon_r', 'kruskal_etasq'), 
         ~mutate(.x, 
                 test_type = ifelse(format == 'numeric', 
                                    .y, 'cramer_v'), 
                 plot_type = ifelse(format == 'numeric', 
                                    'violin', 'stack')))
  
  ## lexicons 
  
  eda_cohort[c("hact_lexicon", "covild_lexicon")] <- 
    map2(list(hact$dict, covild$dict), 
         eda_cohort[c("hact_vars", "covild_vars")],
         inner_join, by = 'variable') %>% 
    map(mutate, 
        y_lab = ifelse(format == 'numeric', axis_lab, '% of strata'))

  eda_cohort[c("hact_vars", "covild_vars")] <- NULL
  
  eda_cohort <- compact(eda_cohort)
  
# descriptive stats, HACT -----
  
  insert_msg('Descritive stats, HACT cohort')
  
  eda_cohort$stats_hact <- 
    explore(eda_cohort$hact_tbl, 
            split_factor = 'cohort', 
            variables = eda_cohort$hact_lexicon$variable, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(eda_cohort$hact_tbl$cohort)))
  
# testing, HACT ----
  
  insert_msg('Testing for the AT/IT differences')
  
  eda_cohort$test_hact <- 
    compare_variables(eda_cohort$hact_tbl, 
                      split_factor = 'cohort', 
                      variables = eda_cohort$hact_lexicon$variable, 
                      what = 'eff_size', 
                      types = eda_cohort$hact_lexicon$test_type, 
                      ci = FALSE, 
                      adj_method = 'BH', 
                      pub_styled = TRUE) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# common result table, HACT -----  
  
  insert_msg('Common reult table for HACT')
  
  eda_cohort$cmm_tbl_hact <- 
    left_join(eda_cohort$stats_hact , 
              eda_cohort$test_hact[c('variable', 
                                     'significance', 
                                     'eff_size')], 
              by = 'variable') %>% 
    mutate(var_r = variable) %>% 
    format_summ_tbl(dict = eda_cohort$hact_lexicon)
  
# HACT, plots -------
  
  insert_msg('HACT plots')
  
  eda_cohort$plots_hact <- 
    list(variable = eda_cohort$test_hact$variable, 
         plot_title = exchange(eda_cohort$test_hact$variable, 
                               dict = eda_cohort$hact_lexicon) %>% 
           paste0(', survey study'), 
         plot_subtitle = eda_cohort$test_hact$plot_cap, 
         y_lab = exchange(eda_cohort$test_hact$variable, 
                          dict = eda_cohort$hact_lexicon, 
                          value = 'y_lab'), 
         type = exchange(eda_cohort$test_hact$variable, 
                         dict = eda_cohort$hact_lexicon, 
                         value = 'plot_type')) %>% 
    pmap(plot_variable, 
         eda_cohort$hact_tbl, 
         split_factor = 'cohort', 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_n_labs = TRUE) %>% 
    map(~.x + theme(axis.title.x = element_blank())) %>% 
    set_names(eda_cohort$test_hact$variable)
  
  ## fill scales
  
  eda_cohort$plots_hact[blast(eda_cohort$hact_lexicon, 'format')[['numeric']]$variable] <- 
    eda_cohort$plots_hact[blast(eda_cohort$hact_lexicon, 'format')[['numeric']]$variable] %>% 
    map(~.x + 
          scale_fill_manual(values = unname(globals$hact_colors)))
  
  eda_cohort$plots_hact[blast(eda_cohort$hact_lexicon, 'format')[['factor']]$variable] <- 
    eda_cohort$plots_hact[blast(eda_cohort$hact_lexicon, 'format')[['factor']]$variable] %>% 
    map(~.x + 
          scale_fill_brewer(palette = 'Reds'))
  
# descriptive stats, CovILD -----
  
  insert_msg('Descriptive stats, CovILD cohort')
    
  eda_cohort$stats_covild <- c(list(mutate(covild$data, cat_WHO = 'cohort')), 
                               blast(covild$data, cat_WHO)) %>% 
    reduce(rbind) %>% 
    mutate(cat_WHO = factor(cat_WHO, c('cohort', 'A', 'HM', 'HS'))) %>% 
    filter(time == 0) %>% 
    explore(split_factor = 'cat_WHO', 
            variables = eda_cohort$covild_lexicon$variable, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'cohort', levels(covild$data$cat_WHO)))
  
# testing, CovILD -----
  
  insert_msg('Testing for the differences in the severity strata, covILD')
  
  eda_cohort$test_covild <- 
    compare_variables(covild$data, 
                      split_factor = 'cat_WHO', 
                      variables = eda_cohort$covild_lexicon$variable, 
                      what = 'eff_size', 
                      types = eda_cohort$covild_lexicon$test_type, 
                      ci = FALSE, 
                      adj_method = 'BH', 
                      pub_styled = TRUE) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# common result table, CovILD ----
  
  insert_msg('Common result table, CovILD')
  
  eda_cohort$cmm_tbl_covild <- left_join(eda_cohort$stats_covild, 
                                         eda_cohort$test_covild[c('variable', 
                                                                  'significance', 
                                                                  'eff_size')], 
                                         by = 'variable') %>% 
    mutate(var_r = variable) %>% 
    format_summ_tbl(dict = eda_cohort$covild_lexicon)
  
# Covild, plots -------
  
  insert_msg('Covild, plots')
  
  eda_cohort$plots_covild <- 
    list(variable = eda_cohort$test_covild$variable, 
         plot_title = exchange(eda_cohort$test_covild$variable, 
                               dict = eda_cohort$covild_lexicon) %>% 
           paste0(', CovILD study'), 
         plot_subtitle = eda_cohort$test_covild$plot_cap, 
         y_lab = exchange(eda_cohort$test_covild$variable, 
                          dict = eda_cohort$covild_lexicon, 
                          value = 'y_lab'), 
         type = exchange(eda_cohort$test_covild$variable, 
                         dict = eda_cohort$covild_lexicon, 
                         value = 'plot_type')) %>% 
    pmap(plot_variable, 
         covild$data, 
         split_factor = 'cat_WHO', 
         scale = 'percent', 
         cust_theme = globals$common_theme, 
         x_n_labs = TRUE) %>% 
    map(~.x + theme(axis.title.x = element_blank())) %>% 
    set_names(eda_cohort$test_covild$variable)
  
  ## fill scales
  
  eda_cohort$plots_covild[blast(eda_cohort$covild_lexicon, 'format')[['numeric']]$variable] <- 
    eda_cohort$plots_covild[blast(eda_cohort$covild_lexicon, 'format')[['numeric']]$variable] %>% 
    map(~.x + 
          scale_fill_manual(values = unname(globals$covild_colors)))
  
  eda_cohort$plots_covild[blast(eda_cohort$covild_lexicon, 'format')[['factor']]$variable] <- 
    eda_cohort$plots_covild[blast(eda_cohort$covild_lexicon, 'format')[['factor']]$variable] %>% 
    map(~.x + 
          scale_fill_brewer(palette = 'Blues'))
  
# END -----

  insert_tail()