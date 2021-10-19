# This script checks for differences in hyposmia resolution kinetic between the sexes in the HACT study

  insert_head()
  
# data container -----
  
  kinet_sex <- list()
  
# globals: analysis table ------
  
  insert_msg('Analysis table')
  
  kinet_sex$analysis_tbl <- kinet$analysis_tbl$survey %>% 
    map(select, 
        ID, time, anosmia) %>% 
    map2(., 
         map(hyposmia[c('survey_at', 'survey_it')], 
             ~.x[c('ID', 'sex')]), 
         left_join, 
         by = 'ID') %>% 
    map(dlply, 'sex', as_tibble) %>% 
    unlist(recursive = F)
  
# kinetic models -----
  
  insert_msg('Kinetic models')
  
  kinet_sex$models <- kinet_sex$analysis_tbl %>% 
    map(model_kinetic, 
        response = 'anosmia', 
        time_var = 'time', 
        family = 'binomial')
  
# LRT -----
  
  insert_msg('LRT')
  
  kinet_sex$lrt <- kinet_sex$models %>% 
    lrt_list(.parallel = T) %>% 
    mutate(cohort = c(rep('survey_at', 6), 
                      rep('survey_it', 6)), 
           sex = c(rep('female', 3), 
                   rep('male', 3), 
                   rep('female', 3), 
                   rep('male', 3)))
  
# inference -----
  
  insert_msg('Inference')

  kinet_sex$beta <- kinet_sex$models %>% 
    inference_kinetic_list(.parallel = T) %>% 
    mutate(cohort = c(rep('survey_at', 6), 
                      rep('survey_it', 6)), 
           sex = c(rep('female', 3), 
                   rep('male', 3), 
                   rep('female', 3), 
                   rep('male', 3)))
  
# single kinetic plots ------
  
  insert_msg('Kinetic plots')
  
  kinet_sex$plots <- list(kinetic_object = kinet_sex$models, 
                          plot_title = c('HACT Austria, female', 
                                         'HACT Austria, male', 
                                         'HACT Italy, female', 
                                         'HACT Italy, male'), 
                          point_color = c('indianred', 
                                          'cornflowerblue', 
                                          'indianred', 
                                          'cornflowerblue'), 
                          resp_color = c('indianred', 
                                         'cornflowerblue', 
                                         'indianred', 
                                         'cornflowerblue'), 
                          fitted_color = c('indianred', 
                                           'cornflowerblue', 
                                           'indianred', 
                                           'cornflowerblue')) %>% 
    pmap(plot, 
         type = 'prevalence', 
         cust_theme = globals$common_theme, 
         x_lab = 'Time after clinical onset, days') %>% 
    map(function(x) x + scale_y_continuous(limits = c(0, 100)))
  
# Forest plot with the beta estimates for males and females ------
  
  insert_msg('Forest plot with the beta estimates in the gender groups')
  
  kinet_sex$beta_plot <- kinet_sex$beta %>% 
    filter(parameter != '(Intercept)') %>% 
    mutate(estimate = exp(estimate), 
           lower_ci = exp(lower_ci), 
           upper_ci = exp(upper_ci), 
           est_txt = paste0(signif(estimate, 2), 
                            ' [', 
                            signif(lower_ci, 2), 
                            ' - ', 
                            signif(upper_ci, 2), 
                            ']')) %>% 
    ggplot(aes(x = estimate, 
               y = sex, 
               color = parameter)) +
    geom_vline(xintercept = 1, 
               linetype = 'dashed') + 
    facet_grid(cohort ~ parameter, 
               labeller = labeller(.rows = globals$cohort_labels, 
                                   .cols = c(time = 'first order\nrecovery', 
                                             time_sqr = 'second order\nchronicity')), 
               scales = 'free') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0) + 
    geom_point(shape = 16) + 
    geom_text(aes(label = est_txt), 
              size = 2.5, 
              hjust = 0, 
              vjust = -0.8) + 
    scale_x_continuous(trans = 'identity') + 
    scale_color_manual(values = c(time = 'steelblue3', 
                                  time_sqr = 'coral3')) + 
    guides(color = F) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          panel.grid.major = element_line(color = 'gray90')) + 
    labs(title = 'HACT: self-reported hyposmia', 
         subtitle = 'Second-order kinetic modeling', 
         x = 'OR')
  
# hyposmia duration in the gender strata, individuals with acute hyposmia -----
  
  insert_msg('Hyposmia median recovery in the gender strata, individuals with acute hyposmia')
  
  ## duration table
  
  kinet_sex$duration$analysis_tbl <- kinet_sex$analysis_tbl %>% 
    map(calculate_duration, 
        symptom = 'anosmia') %>% 
    list(table = ., 
         cohort_vec = c(rep('survey_at', 2), 
                        rep('survey_it', 2)), 
         sex_vec = c('female', 
                     'male', 
                     'female', 
                     'male')) %>% 
    pmap_dfr(function(table, cohort_vec, sex_vec) mutate(table, 
                                                         sex = sex_vec, 
                                                         cohort = cohort_vec)) %>% 
    filter(anosmia != 0)

  ## duration stats
  
  kinet_sex$duration$duration_stats <- kinet_sex$duration$analysis_tbl %>% 
    group_by(cohort, sex) %>% 
    summarize(median = median(anosmia, na.rm = T), 
              perc25 = quantile(anosmia, 0.25, na.rm = T), 
              perc75 = quantile(anosmia, 0.75, na.rm = T))
  
  ## analysis and summary
  
  kinet_sex$duration$analysis <- kinet_sex$duration$analysis_tbl %>% 
    dlply(.(cohort)) %>% 
    map(analyze_feature, 
        variable = 'anosmia', 
        split_var = 'sex')
  
  kinet_sex$duration$summary <- kinet_sex$duration$analysis %>% 
    map_dfr(extract_test_summary) %>% 
    filter(test == 'u') %>% 
    mutate(p_adj = p.adjust(p_value, 'BH'))
  
  ## plots
  
  kinet_sex$duration$plots <- list(analysis_object = kinet_sex$duration$analysis, 
                                   label = c('HACT Austria', 'HACT Italy')) %>% 
    pmap(plot_analysis, 
         fill_colors = c('indianred', 'cornflowerblue'), 
         violin = T, 
         cust_theme = globals$common_theme, 
         y_lab = 'Hyposmia, recovery time, days', 
         show_points = F) %>% 
    map2(., kinet_sex$duration$summary$p_adj, 
         function(x, y) x + 
           labs(subtitle = paste('p =', signif(y, 2))) + 
           theme(legend.position = 'none') + 
           scale_y_continuous(breaks = c(0, 7, 14, 28, 90)))
  
# comparison of the initial hyposmia frequency during acute COVID-19 in males and females -----
  
  insert_msg('Differences in frequency of hyposmia during acute COVID in males and females')
  
  ## analyses
  
  kinet_sex$acute_covid$analyses <- list(survey_at = rbind(kinet_sex$analysis_tbl$survey_at.female, 
                                                           kinet_sex$analysis_tbl$survey_at.male), 
                                         survey_it = rbind(kinet_sex$analysis_tbl$survey_it.female, 
                                                           kinet_sex$analysis_tbl$survey_it.male)) %>% 
    map(filter, 
        time == 0) %>% 
    map(analyze_feature, 
        variable = 'anosmia', 
        split_var = 'sex')
  
  ## counts and test results
  
  kinet_sex$acute_covid$counts <- kinet_sex$acute_covid$analyses %>% 
    map(extract_counts) %>% 
    map(filter, 
        strata == 'yes')
  
  kinet_sex$acute_covid$test <- kinet_sex$acute_covid$analyses %>% 
    map(extract_test_summary)
  
# END -----
  
  insert_tail()