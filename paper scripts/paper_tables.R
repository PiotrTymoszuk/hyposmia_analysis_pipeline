# This script generates supplementary tables 

  insert_head()
  
# data container -----
  
  paper_tables <- list()
  suppl_tables <- list()
  
# Tables 1 and 2: characteristic of the CovILD and HACT cohorts ------
  
  insert_msg('Tables 1 and 2: study cohort characteristic')
  
  paper_tables[c('cohort_covild', 
                 'cohort_survey')] <- cohort$raw_tbl %>% 
    map2(., list(c('Parameter', 'Ambulatory', 'Moderate', 'Severe', 'Test', 'Significance'), 
                 c('Parameter', 'Austria', 'Italy', 'Test', 'Significance')), 
         set_names)
  
  paper_tables$cohort_covild <- paper_tables$cohort_covild %>% 
    mutate(Test = ifelse(Test == 'U', 'KW', Test))
  
# Supplementary Table S1: study variables -----
  
  insert_msg('Table S1: study variables')
  
  suppl_tables$study_vars$survey <- globals$survey_var_lexicon %>% 
    mutate(study = 'HACT', 
           timepoint = NA)
  
  suppl_tables$study_vars$covild <- globals$var_lexicon %>% 
    mutate(variable = new_name, 
           timepoint = car::recode(reference, 
                                   "0 = 'acute COVID-19'; 
                                   1 = '60 day FUP'; 
                                   2 = '100 day FUP'; 
                                   3 = '180 day FUP'; 
                                   4 = '100 day FUP'; 
                                   5 = '100 day FUP'"), 
           study = 'CovILD', 
           description = label, 
           cutpoints = NA, 
           levels = NA, 
           modeling_variable = ifelse(var_type != 'independent' | is.na(var_type), 'no', 'yes'))
  
  suppl_tables$study_vars <- suppl_tables$study_vars %>% 
    map(select, 
        variable, 
        label, 
        unit, 
        description, 
        cutpoints, 
        levels, 
        modeling_variable, 
        study, 
        timepoint) %>% 
    reduce(rbind) %>% 
    set_names('Variable in R code', 
              'Label', 
              'Unit', 
              'Description', 
              'Cutoffs', 
              'Levels', 
              'Independent modeling variable', 
              'Study', 
              'Time point')

# Supplementary Table S2: results of kinetic modeling -----
  
  insert_msg('Table S2: results of kinetic modeling')
  
  suppl_tables$kinetic_modeling <- kinet$summary %>% 
    map(function(x) x %>% 
          map2_dfr(., names(.), ~mutate(.x, subset = .y))) %>% 
    map2(., globals[c('var_lexicon', 
                      'survey_var_lexicon')], 
         ~mutate(.x, response = translate_var(response, 
                                              dict = .y, 
                                              time_lab = F))) %>% 
    map2_dfr(., c('CovILD', 'HACT'), 
             ~mutate(.x, study = .y)) %>% 
    mutate(term = car::recode(term, "'order_0' = 'null'; 'order_1' = 'first'; 'order_2' = 'second'"), 
           subset = car::recode(subset, "'A' = 'ambulatory'; 'HM' = 'moderate'; 'HS' = 'severe'; 
                                'survey_at' = 'Austria'; 'survey_it' = 'Italy'"), 
           estimate = paste0(signif(estimate, 2), 
                             ' [', 
                             signif(lower_ci, 2), 
                             ' - ', 
                             signif(upper_ci, 2), 
                             ']'), 
           signif_ols = ifelse(p_adj_ols < 0.05, 
                               paste('p =', signif(p_adj_ols, 2)), 
                               'ns'), 
           deviance = signif(deviance, 3), 
           signif_lrt = ifelse(p_adj_lrt < 0.05, 
                               paste('p =', signif(p_adj_lrt, 2)), 
                                     'ns')) %>% 
    select(study, 
           subset, 
           response, 
           n_number, 
           term, 
           estimate, 
           signif_ols, 
           deviance, 
           signif_lrt) %>%
  set_names(c('Study', 
              'Subset', 
              'Modeling response', 
              'N complete observations', 
              'Time term', 
              'OR', 
              'Significance OR', 
              'Deviance', 
              'Significance LRT'))
  
# Supplementary Table S3: cosine similarity between the self-reported hyposmia and other symptoms -----
  
  insert_msg('Table S3: similarities between hyposmia and other symptoms')
  
  suppl_tables$hyposmia_similarity$covild <- hypo_overlap$distances$covild %>% 
    map2_dfr(., c('ambulatory', 'moderate', 'severe'), ~mutate(.x, subset = .y))
  
  suppl_tables$hyposmia_similarity$survey <- hypo_overlap$distances$survey %>% 
    map2_dfr(., c('Austria', 'Italy'), ~mutate(.x, subset = .y))
  
  suppl_tables$hyposmia_similarity <- suppl_tables$hyposmia_similarity %>% 
    reduce(rbind) %>% 
    select(var_label, time, stat) %>% 
    set_names('Symptom', 'Time point', 'Cosine similarity')
  
# Supplementary Table S4: results of risk modeling -----
  
  insert_msg('Table S4: results of risk modeling')
  
  suppl_tables$uni_modeling$covild <- uni_model$summary$covild %>% 
    reduce(rbind) %>% 
    mutate(study = 'CovILD', 
           subset = NA)
  
  suppl_tables$uni_modeling$survey <- uni_model$summary$survey %>% 
    map2_dfr(., c('Austria', 'Italy'), ~mutate(.x, 
                                               study = 'HACT', 
                                               subset = .y))
  
  suppl_tables$uni_modeling <- suppl_tables$uni_modeling %>% 
    reduce(rbind) %>% 
    mutate(response = car::recode(response, 
                                  "'anosmia_sympt_V2' = 'Self-reported hyposmia @100 day FUP';
                                  'hyposmia_mod_severe_V2' = 'Sniffing test < 13 pt @ 100 day FUP'; 
                                  'anosmia_chronic' = 'Self-reported hyposmia @90 day after clinical onset'"), 
           estimate = paste0(signif(estimate, 2), 
                             ' [', 
                             signif(lower_ci, 2), 
                             ' - ', 
                             signif(upper_ci, 2), 
                             ']'), 
           significance = ifelse(p_adj >= 0.05, 'ns', paste('p =', signif(p_adj, 2)))) %>% 
    select(study, 
           subset, 
           response, 
           level, 
           n_complete, 
           estimate, 
           significance) %>% 
    set_names(c('Study', 
                'Subset', 
                'Modeling response', 
                'Response level', 
                'N complete observations', 
                'OR', 
                'Significance OR'))
  
# saving the tables on the disc ----
  
  insert_msg('Saving the tables on the disc')
  
  suppl_tables %>% 
    set_names(paste0('Table ', 
                     1:length(suppl_tables))) %>% 
    write_xlsx(path = './paper/tables.xlsx')
  
  suppl_tables %>% 
    set_names(paste0('Table S', 
                     1:length(suppl_tables))) %>% 
    write_xlsx(path = './paper/supplementary_tables.xlsx')
  
# END -----
  
  insert_tail()