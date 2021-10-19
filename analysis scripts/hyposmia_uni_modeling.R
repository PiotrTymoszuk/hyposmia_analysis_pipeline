# This script performs univariate logistic modeling of the self-reported and test hyposmia

  insert_head()
  
# data container -----
  
  uni_model <- list()
  
# globals: independent variables and model responses -----
  
  insert_msg('Globals setup')
  
  ## responses 
  
  uni_model$responses$covild <- c('anosmia_sympt_V2', 
                                  'hyposmia_mod_severe_V2') %>% 
    set_names(c('self', 'test'))
    
  
  uni_model$responses$survey <- 'anosmia_chronic'
  
  ## independent variables
  
  uni_model$variables$covild <- globals$variables$new_name
  
  uni_model$variables$survey <- globals$survey_var_lexicon %>% 
    filter(modeling_variable == 'yes', 
           variable != 'obs_time_strata') %>% 
    .$variable
  
# serial modeling -----
  
  insert_msg('Serial modeling')
  
  ## covild: seld-reported and test hyposmia
  
  uni_model$models$covild <- uni_model$responses$covild %>% 
    map(make_lm_model, 
        data = hyposmia$covild, 
        indep_variable = uni_model$variables$covild, 
        mod_fun = glm, 
        family = 'binomial', 
        est_transf = exp, 
        error_resistant = T) 

  ## survey
  
  uni_model$models$survey <- hyposmia[c('survey_at', 'survey_it')] %>% 
    map(make_lm_model, 
        response = uni_model$responses$survey, 
        indep_variable = uni_model$variables$survey, 
        weight_variable = 'freq_weight', 
        confounder = 'obs_time_strata', 
        mod_fun = glm, 
        family = 'quasibinomial', 
        est_transf = exp, 
        error_resistant = T)
  
# model summaries -----
  
  insert_msg('Modeling summaries')

  ## summary, all models
  
  uni_model$summary <- uni_model$models %>% 
    map(function(x) x %>% 
          map(get_model_summary) %>% 
          map(filter, 
              variable != 'obs_time_strata') %>% 
          map(mutate, 
              significant = ifelse(p_adj < 0.05, 'yes', 'no'), 
              correlation = ifelse(significant == 'no', 
                                   'ns', 
                                   ifelse(estimate > 1, 'positive', 'negative'))))
  
  ## variable translation
  
  uni_model$summary$covild <- uni_model$summary$covild  %>% 
    map(mutate, 
        var_label = translate_var(variable, 
                                  key = 'new_name', 
                                  dict = globals$var_lexicon, 
                                  time_lab = T))
  
  uni_model$summary$survey <- uni_model$summary$survey  %>% 
    map(mutate, 
        var_label = translate_var(variable, 
                                  out_value = 'label_short', 
                                  dict = globals$survey_var_lexicon, 
                                  time_lab = F))

  ## summary, significant correlates
  
  uni_model$signif <- uni_model$summary %>% 
    map(function(x) x %>% 
          map(filter, 
              level != 'baseline', 
              correlation != 'ns'))
  
# plotting the nearly significant results -----
  
  insert_msg('Plotting of the significant estimates')
  
  ## COVILD
  
  uni_model$forest_plots$covild <- list(uni_modeling_summary = map(uni_model$summary$covild, 
                                                                   filter, 
                                                                   p_adj < 0.1, 
                                                                   level != 'baseline'), 
                                        plot_title = c('CovILD, hypo/anosmia', 
                                                       'CovILD, sniffing-test < 13 pt')) %>% 
    pmap(plot_uni_forest, 
         plot_subtitle = 'Univariate logistic modeling')
  
  ## survey
  
  uni_model$forest_plots$survey <- list(uni_modeling_summary = map(uni_model$summary$survey, 
                                                                   filter, 
                                                                   p_adj < 0.1, 
                                                                   level != 'baseline'), 
                                        plot_title = c('HACT: Austria, hypo/anosmia', 
                                                       'HACT: Italy, hypo/anosmia')) %>% 
    pmap(plot_uni_forest, 
         plot_subtitle = 'Univariate logistic modeling')
    
# END ----
  
  insert_tail()