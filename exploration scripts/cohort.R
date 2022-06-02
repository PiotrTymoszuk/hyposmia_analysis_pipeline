# Characteristic of the study cohorts (HACT and CovILD): baseline, 
# clinical history, CoV course and recovery

  insert_head()
  
# container list ------
  
  eda_cohort <- list()
  
# globals ------
  
  insert_msg('Globals setup')
  
  ## analysis table for covild
  
  eda_cohort$hact_tbl <- hact[c('north', 'south')] %>% 
    reduce(rbind)
  
  ## test variables for HACT
  
  eda_cohort$hact_vars$variable <- globals[c('demo_vars', 
                                             'clinic_vars', 
                                             'cov_vars', 
                                             'psych_var')] %>% 
    reduce(c)
  
  eda_cohort$hact_vars$test_type <- eda_cohort$hact_vars$variable %>% 
    map_chr(~ifelse(is.numeric(hact$north[[.x]]), 'wilcoxon_r', 'cramer_v'))

  eda_cohort$hact_vars <- as_tibble(eda_cohort$hact_vars)
  
  ## test variables for CovILD
  
  eda_cohort$covild_vars$variable <- globals$covild_vars
  
  eda_cohort$covild_vars$test_type <- eda_cohort$covild_vars$variable %>% 
    map_chr(~ifelse(is.numeric(covild$data[[.x]]), 'kruskal_test', 'chisq_test'))
  
  eda_cohort$covild_vars <- as_tibble(eda_cohort$covild_vars)
  
# descriptive stats, HACT -----
  
  insert_msg('Descritive stats, HACT cohort')
  
  eda_cohort$stats_hact <- explore(eda_cohort$hact_tbl, 
                                   split_factor = 'cohort', 
                                   variables = eda_cohort$hact_vars$variable, 
                                   what = 'table', 
                                   pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'north', 'south'))
  
# testing, HACT ----
  
  insert_msg('Testing for the AT/IT differences')
  
  eda_cohort$test_hact <- compare_variables(eda_cohort$hact_tbl, 
                                            split_factor = 'cohort', 
                                            variables = eda_cohort$hact_vars$variable, 
                                            what = 'eff_size', 
                                            types = eda_cohort$hact_vars$test_type, 
                                            ci = FALSE, 
                                            adj_method = 'BH', 
                                            pub_styled = TRUE)
  
# common result table, HACT -----  
  
  insert_msg('Common reult table for HACT')
  
  eda_cohort$cmm_tbl_hact <- left_join(eda_cohort$stats_hact , 
                                       eda_cohort$test_hact[c('variable', 
                                                              'significance', 
                                                              'eff_size')], 
                                       by = 'variable') %>% 
    mutate(var_r = variable) %>% 
    format_summ_tbl(dict = hact$dict)
  
# descriptive stats, CovILD -----
  
  insert_msg('Descriptive stats, CovILD cohort')
    
  eda_cohort$stats_covild <- c(list(mutate(covild$data, cat_WHO = 'cohort')), 
                               dlply(covild$data, 'cat_WHO')) %>% 
    reduce(rbind) %>% 
    mutate(cat_WHO = factor(cat_WHO, c('cohort', 'A', 'HM', 'HS'))) %>% 
    filter(time == 0) %>% 
    explore(split_factor = 'cat_WHO', 
            variables = eda_cohort$covild_vars$variable, 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', 'cohort', 'A', 'HM', 'HS'))
  
# testing, CovILD -----
  
  insert_msg('Testing for the differences in the severity strata, covILD')
  
  eda_cohort$test_covild <- compare_variables(covild$data, 
                                              split_factor = 'cat_WHO', 
                                              variables = eda_cohort$covild_vars$variable, 
                                              what = 'test', 
                                              types = eda_cohort$covild_vars$test_type, 
                                              ci = FALSE, 
                                              adj_method = 'BH', 
                                              pub_styled = TRUE)
  
# common result table, CovILD ----
  
  insert_msg('Common result table, CovILD')
  
  eda_cohort$cmm_tbl_covild <- left_join(eda_cohort$stats_covild, 
                                         eda_cohort$test_covild[c('variable', 
                                                                  'significance', 
                                                                  'eff_size')], 
                                         by = 'variable') %>% 
    mutate(var_r = variable) %>% 
    format_summ_tbl(dict = covild$dict)
  
# END -----
  
  insert_tail()