# This script stitches together the cohort characteristic 


  insert_head()
  
# data container ----
  
  cohort <- list()
  
# globals: variables of interest -----
  
  insert_msg('Globals setup')
  
  ## CovILD
  
  cohort$variables$covild <- c('sex', 
                               'age', 
                               'age_class',
                               'smoking', 
                               'comorb_present', 
                               'no_comorb', 
                               'weight_class',
                               'cardiovascular_comorb', 
                               'hypertension_comorb', 
                               'pulmonary_comorb', 
                               'endometabolic_comorb', 
                               'hyperchol_comorb', 
                               'diabetes_comorb', 
                               'malingancy_comorb', 
                               'sympt_present_V0', 
                               'sympt_number_V0')
  
  ## Survey
  
  cohort$variables$survey <- c('obs_time', 
                               'obs_time_strata', 
                               'sex', 
                               'age', 
                               'age_class', 
                               'smoking', 
                               'comorb_absent', 
                               'comorb_sum', 
                               'bmi_class_before', 
                               'heart_circulation', 
                               'hypertension', 
                               'lung', 
                               'hay_fever', 
                               'diabetes', 
                               'malignancy', 
                               'acute_covid', 
                               'sum_symptoms_acute', 
                               'sum_symptoms_acute_class')
  
# serial analysis -----
  
  insert_msg('Serial analysis')
  
  ## covild
  
  cohort$analysis$covild <- cohort$variables$covild %>% 
    map(analyze_feature, 
        inp_tbl = hyposmia$covild, 
        split_var = 'cat_WHO') %>% 
    set_names(cohort$variables$covild)
  
  ## survey
  
  cohort$analysis$survey <- cohort$variables$survey %>% 
    map(analyze_feature, 
        inp_tbl = map2_dfr(hyposmia[c('survey_at', 'survey_it')],
                           c('Austria', 'Italy'), 
                           ~mutate(.x, cohort = .y)) %>% 
          mutate(cohort = factor(cohort)),  
        split_var = 'cohort') %>% 
    set_names(cohort$variables$survey)
  
# Raw tables -----
  
  insert_msg('Pasting the raw tables together')

  ## CovILD
  
  cohort$raw_tbl$covild <- get_feature_summary(analysis_object = cohort$analysis$covild, 
                                               label = translate_var(cohort$variables$covild, 
                                                                     dict = globals$var_lexicon, 
                                                                     time_lab = T, 
                                                                     key = 'new_name'))
  
  ## Survey
  
  cohort$raw_tbl$survey <- get_feature_summary(analysis_object = cohort$analysis$survey, 
                                               label = translate_var(cohort$variables$survey, 
                                                                     dict = globals$survey_var_lexicon, 
                                                                     time_lab = F))

# Clearing the tables -----
  
  insert_msg('Clearing the tables')
  
  cohort$raw_tbl <- cohort$raw_tbl %>% 
    map(mutate, 
        significance = ifelse(is.na(p_chi), p_non_param, p_chi), 
        test = ifelse(is.na(p_chi), 'U', 'Chi'), 
        significance = ifelse(significance >= 0.05, 'ns', paste('p =', signif(significance, 2))), 
        label = car::recode(label, 
                            "'# comorbidities' = 'Number of comorbidities (max. 17)'; 
                            'CVD' = 'Cardiovascular comorbidity'; 
                            'PulmoDis' = 'Pulmonary comorbidity'; 
                            'Endocrine/Metabolic' = 'Endocrine or metabolic comorbidity'; 
                            'Hypercholesterol' = 'Hypercholesterolemia'; 
                            'Sympt. present@V0' = 'Symptomatic SARS-CoV-2 infection'; 
                            '# symptoms@V0' = 'Acute COVID-19 symptom number'; 
                            'Sum of co-morbidities' = 'Number of comorbidities (max. 25)'; 
                            'Smoking history' = 'Smoking'; 
                            'Weight class' = 'BMI class'; 
                            'BMI before COVID-19' = 'BMI class'; 
                            'Cardiovascular disease' = 'Cardiovascular comorbidity'; 
                            'Pulmonary disease' = 'Pulmonary comorbidity'; 
                            'Acute COVID-19 symptoms' = 'Symptomatic SARS-CoV-2 infection'; 
                            '# acute symptoms' = 'Acute COVID-19 symptom number'; 
                            'Co-morbidity absent' = 'Comorbidity absent'")) %>% 
    map(select, 
        label, 
        any_of(c('A', 'HM', 'HS', 'Austria', 'Italy')), 
        test, 
        significance) %>% 
    map(function(x) x %>% 
          map_dfr(stri_replace, 
                  regex = '^no:.*\nyes:\\s{1}', 
                  replacement = '') %>% 
          map_dfr(stri_replace, 
                  fixed = 'complete', 
                  replacement = ''))
  
# END ----
  
  insert_tail()
  