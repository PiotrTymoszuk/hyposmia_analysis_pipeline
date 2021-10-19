# This script analyses symptom kinetic in the severity groups in the COVILD study and the Health after COVID cohorts

  insert_head()
  
# data container ----
  
  kinet <- list()
  
# COVILD analysis table and variables ----
  
  insert_msg('Setting up the COVILD globals')
  
  ## variables 
  
  kinet$variables$covild <- globals$var_lexicon %>% 
    filter(stri_detect(new_name, regex = 'sympt_.*'), 
           !stri_detect(new_name, fixed = 'number')) %>% 
    .$new_name
  
  ## analysis table
  
  kinet$analysis_tbl$covild <- hyposmia$covild %>% 
    select(ID, 
           all_of(kinet$variables$covild)) %>% 
    gather(key = symptom, 
           value = present,
           all_of(kinet$variables$covild)) %>% 
    mutate(time = stri_extract(symptom, regex = 'V.*'), 
           time = car::recode(time, "'V0' = 0; 'V1' = 60; 'V2' = 100; 'V3' = 180") %>% 
             as.numeric, 
           symptom = stri_replace(symptom, regex = '_V.*', replacement = ''), 
           present = factor(present)) %>% 
    spread(key = 'symptom',
           value = 'present')
  
  kinet$variables$covild <- stri_replace(kinet$variables$covild, 
                                         regex = '_V.*', 
                                         replacement = '') %>% 
    unique

  ## splitting the analysis table according to the WHO severity
  
  kinet$analysis_tbl$covild <- levels(hyposmia$covild$cat_WHO) %>% 
    map(function(x) left_join(kinet$analysis_tbl$covild, 
                              hyposmia$covild[c('ID', 'cat_WHO')], 
                              by = 'ID') %>% 
          filter(cat_WHO == x)) %>% 
    set_names(levels(hyposmia$covild$cat_WHO))
  
# HACT analysis table and variables -----
  
  insert_msg('Setting up the survey globals')
  
  ## variables
  
  kinet$variables$survey <- globals$symptoms %>% 
    map(paste, 
        c('acute', 'immediate', 'subacute', 'long', 'chronic'), 
        sep = '_') %>% 
    reduce(c)
  
  ## analysis table
  
  kinet$analysis_tbl$survey <- hyposmia[c('survey_at', 'survey_it')] %>% 
    map(select, 
        ID, 
           all_of(kinet$variables$survey)) %>% 
    map(gather, key = symptom, 
           value = present,
           all_of(kinet$variables$survey)) %>% 
    map(mutate, 
        present = factor(present), 
        time = stri_extract(symptom, 
                            regex = paste(c('acute', 'immediate', 'subacute', 'long', 'chronic'), collapse = '|')), 
        time = car::recode(time, "'acute' = 0; 'immediate' = 7; 'subacute' = 14; 'long' = 28; 'chronic' = 90") %>% 
          as.numeric, 
        symptom = stri_replace(symptom, 
                               regex = paste(c('_acute', '_immediate', '_subacute', '_long', '_chronic'), collapse = '|'), 
                               replacement = '')) %>% 
    map(spread, 
        key = 'symptom', 
        value = 'present')
  
  kinet$variables$survey <- globals$symptoms
  
# kinetic modeling -----
  
  insert_msg('Kinetic modeling')
  
  ## COVILD
  
  kinet$models$covild <- kinet$analysis_tbl$covild %>% 
    map(model_kinetic_lst, 
        responses = kinet$variables$covild, 
        time_var = 'time', 
        ID_var = 'ID', 
        family = 'binomial', 
        order = 2, 
        .parallel = F)

  ## survey
  
  kinet$models$survey <- kinet$analysis_tbl$survey %>% 
    map(model_kinetic_lst, 
        responses = kinet$variables$survey, 
        time_var = 'time', 
        ID_var = 'ID', 
        family = 'binomial', 
        order = 2, 
        .parallel = T)

# LRT testing, preparing the plot captions with the BH corrected p values -----
  
  insert_msg('LRT testing')
  
  ## COVILD
  
  kinet$lrt$covild <- kinet$models$covild %>% 
    map(lrt_list, 
        .parallel = F)

  ## survey
  
  plan('multisession')

  kinet$lrt$survey <- kinet$models$survey %>% 
    map(lrt_list, 
        .parallel = T)

  ##  extracting the plot tags
  
  kinet$lrt_captions <- kinet$lrt %>% 
    map(function(x) x %>% map(get_lrt))

# Betas, preparing the plot captions with the betas----
  
  insert_msg('Model inference')
  
  ## COVILD

  kinet$beta$covild <- kinet$models$covild %>% 
    map(inference_kinetic_list, 
        .parallel = T)
  
  ## survey

  kinet$beta$survey <- kinet$models$survey %>% 
    map(inference_kinetic_list, 
        .parallel = T)

  ## plot tags
  
  kinet$beta_captions <- kinet$beta %>% 
    map(function(x) x %>% map(get_caption, p_only = F))
  
# Merging the LRT and inference summaries ----
  
  insert_msg('Merging the LRT and inference summaries')
  
  kinet$summary <- map2(kinet$beta, 
                        kinet$lrt, 
                        function(x, y) list(ols_summary = x, 
                                            lrt_summary = y) %>% 
                          pmap(cmm_kinet_summary, 
                               estimate_trans = exp))
  
  ## adding the response labels
  
  kinet$summary$covild <- kinet$summary$covild %>% 
    map(mutate, 
        resp_label = translate_var(response, 
                                   dict = globals$var_lexicon, 
                                   time_lab = F))
  
  kinet$summary$survey <- kinet$summary$survey %>% 
    map(mutate, 
        resp_label = translate_var(response, 
                                 dict = globals$survey_var_lexicon, 
                                 time_lab = F))

# Identifying top 10 symptoms with the most signifincat second-order terms (LRT) -----
  
  insert_msg('Identifying top 10 candidate chronic symptoms')
  
  kinet$top_second_order <- kinet$summary %>% 
    map(function(x) x %>% 
          map(filter, 
              term == 'order_2', 
              !is.na(p_adj_lrt)) %>% 
          map(top_n, 
              10, 
              -p_adj_lrt) %>% 
          map(~.x$response))
  
# summary plots with the LRT p values for the recovery and plateau effects ------
  
  insert_msg('Summary plots with the LRT p values')
  
  ## Covild
  
  kinet$lrt_summ_plots$covild <- list(common_kinet_summary = kinet$summary$covild, 
                                      plot_title = globals$sev_labels[names(kinet$summary$covild)], 
                                      signif_color = globals$sev_colors[names(kinet$summary$covild)]) %>% 
    pmap(plot_lrt_summ, 
         plot_subtitle = 'Second order kinetic modeling')
  
  ## Survey
  
  kinet$lrt_summ_plots$survey <- list(common_kinet_summary = kinet$summary$survey, 
                                      plot_title = globals$cohort_labels[names(kinet$summary$survey)], 
                                      signif_color = globals$cohort_colors[names(kinet$summary$survey)]) %>% 
    pmap(plot_lrt_summ, 
         plot_subtitle = 'Second order kinetic modeling')
  
# summary Forest plots with the estimate values for the top 10 candidates for chronic symptoms (LRT second order term) ------
  
  insert_msg('Summary Forests with the beta values')
  
  ## Covild
  
  kinet$beta_summ_plots$covild <- list(common_kinet_summary = kinet$summary$covild %>% 
                                         map2(., kinet$top_second_order$covild, 
                                              function(x, y) filter(x, response %in% y)), 
                                       plot_title = globals$sev_labels[names(kinet$summary$covild)]) %>% 
    pmap(plot_beta_summ, 
         x_trans = 'log2', 
         signif_only = F, 
         show_est_txt = T, 
         plot_subtitle = 'Second order kinetic modeling')
  
  ## survey, the features significant in the LRT test
  
  kinet$beta_summ_plots$survey <- list(common_kinet_summary = kinet$summary$survey %>% 
                                         map2(., kinet$top_second_order$survey, 
                                              function(x, y) filter(x, response %in% y)), 
                                       plot_title = globals$cohort_labels[names(kinet$summary$survey)]) %>% 
    pmap(plot_beta_summ, 
         x_trans = 'log2', 
         signif_only = F, 
         show_est_txt = T, 
         plot_subtitle = 'Second order kinetic modeling')

# recovery plots kinetic plots for the single symptoms -----
  
  insert_msg('Recovery curves for the particular symptoms')
  
  ## COVILD
  
  kinet$kinet_plots$covild <- names(kinet$models$covild) %>% 
    map(function(x) list(kinetic_object = kinet$models$covild[[x]], 
                         point_color = globals$sev_colors[[x]], 
                         resp_color = globals$sev_colors[[x]], 
                         fitted_color = globals$sev_colors[[x]], 
                         plot_title = paste(globals$sev_labels[[x]], 
                                            translate_var(names(kinet$models$covild[[x]]), 
                                                          dict = globals$var_lexicon, 
                                                          time_lab = F), 
                                            sep = ': ')) %>% 
          pmap(plot, 
               type = 'prevalence', 
               cust_theme = globals$common_theme, 
               x_lab = 'Time after clinical onset, days')) %>% 
    set_names(names(kinet$models$covild))
  
  ## survey
  
  kinet$kinet_plots$survey <- names(kinet$models$survey) %>% 
    map(function(x) list(kinetic_object = kinet$models$survey[[x]], 
                         point_color = globals$cohort_colors[[x]], 
                         resp_color = globals$cohort_colors[[x]], 
                         fitted_color = globals$cohort_colors[[x]], 
                         plot_title = paste(globals$cohort_labels[[x]], 
                                            translate_var(names(kinet$models$survey[[x]]), 
                                                          dict = globals$survey_var_lexicon, 
                                                          time_lab = F), 
                                            sep = ': ')) %>% 
          pmap(plot, 
               type = 'prevalence', 
               cust_theme = globals$common_theme, 
               x_lab = 'Time after clinical onset, days')) %>% 
    set_names(names(kinet$models$survey))

  
# END ----
  
  insert_tail()