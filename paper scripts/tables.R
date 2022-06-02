# Paper tables

  insert_head()
  
# container lists ------
  
  paper_tbl <- list()
  suppl_tbl <- list()
  
# globals ------
  
  paper_tbl$hact_base_vars <- c('obs_time', 'sex', 'age', 'bmi_class_before', 
                                'education_class', 'employment_before', 
                                'comorb_present', 'hypertension', 'heart_circulation', 
                                'diabetes', 'lung', 'gastrointenstinal', 
                                'malignancy', 'hay_fever', 'autoimmunity', 
                                'frequent_flu_like', 'two_plus_infections_antibiotics', 
                                'depression_burnout', 'insomnia', 'daily_medication')
  
# Table 1: baseline characteristic of the HACT study cohorts -----
  
  insert_msg('Table 1: baseline, HACT')
  
  paper_tbl$hact_baseline <- eda_cohort$cmm_tbl_hact %>% 
    filter(var_r %in% paper_tbl$hact_base_vars) %>% 
    mutate(variable = translate_var(var_r, out_value = 'label', dict = hact$dict), 
           unit = translate_var(var_r, out_value = 'unit', dict = hact$dict), 
           variable = ifelse(!is.na(unit), 
                             paste(variable, unit, sep = ', '), 
                             variable)) %>% 
    select(variable, north, south, significance, eff_size) %>% 
    set_names(c('Variable', 'AT', 'IT', 'Significance', 'Effect size'))
  
# Table 2: baseline characteristic of the CovILD cohort ------
  
  insert_msg('Table 2: baseline, CovILD')
  
  paper_tbl$covild_baseline <- eda_cohort$cmm_tbl_covild %>% 
    filter(var_r != 'no_comorb') %>% 
    mutate(variable = translate_var(var_r, out_value = 'label', dict = covild$dict), 
           unit = translate_var(var_r, out_value = 'unit', dict = covild$dict), 
           variable = ifelse(is.na(unit) | unit == '%', 
                             variable, 
                             paste(variable, unit, sep = ', '))) %>% 
    mutate(variable = car::recode(variable, 
                                  "'Weight class' = 'BMI at CoV onset'; 
                                  'CVD' = 'Cardiovascular disease'; 
                                  'GID' = 'Gastrointestinal disease'")) %>% 
    select(variable, cohort, A, HM, HS, significance, eff_size) %>% 
    set_names(c('Variable', 
                'Entire cohort', 
                'Ambulatory CoV subset', 
                'Moderate CoV subset', 
                'Severe CoV subset', 
                'Significance', 
                'Effect size'))
  
# Supplementary Table S1: HACT study variables ------
  
  insert_msg('Table S1: HACT study variables')
  
  suppl_tbl$hact_vars <- hact$dict %>% 
    mutate(variable = factor(variable, 
                             c('ID', 
                               'cohort', 
                               'acute_covid', 
                               globals$demo_vars, 
                               globals$clinic_var, 
                               globals$cov_vars, 
                               globals$psych_var, 
                               globals$hact_sympt_class$variable))) %>% 
    filter(!is.na(variable)) %>% 
    arrange(variable) %>% 
    select(variable, 
           label, 
           unit, 
           cutpoints, 
           description) %>% 
    set_names(c('Variable name', 
                'Variable label', 
                'Unit', 
                'Stratification', 
                'Description'))
  
# Supplementary Table S2: CovILD study variables -----
  
  insert_msg('Table S2: CovILD study variables')
  
  suppl_tbl$covild_vars <- covild$dict %>% 
    mutate(variable = factor(variable, 
                             c('ID', 
                               'time_numeric', 
                               globals$covild_vars, 
                               globals$covild_symptoms))) %>% 
    filter(!is.na(variable)) %>% 
    arrange(variable) %>% 
    select(variable, label, unit) %>% 
    set_names(c('Variable name', 'Variable label', 'Unit'))
  
# Supplementary Table S3: Apriori results, long CoV -----
  
  insert_msg('Table S3: apriori results, long COVID')
  
  suppl_tbl$apriori_long <- ap_sympt$rules_tbl[c('north.28', 'south.28')] %>% 
    map2_dfr(., c('AT', 'IT'), ~mutate(.x, cohort = .y, time = 'long COVID')) %>% 
    select(cohort, time, trans_lab, support, confidence, lift) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 3) else x) %>% 
    arrange(-support) %>% 
    set_names(c('Cohort', 'Time', 'Symptoms', 'Support', 'Confidence', 'Lift'))
  
# Supplementary Table S4: Apriori results, PASC -----
  
  insert_msg('Table S4: apriori results, PASC')
  
  suppl_tbl$apriori_pasc <- ap_sympt$rules_tbl[c('north.90', 'south.90')] %>% 
    map2_dfr(., c('AT', 'IT'), ~mutate(.x, cohort = .y, time = 'PASC')) %>% 
    select(cohort, time, trans_lab, support, confidence, lift) %>% 
    map_dfc(function(x) if(is.numeric(x)) signif(x, 3) else x) %>% 
    arrange(-support) %>% 
    set_names(c('Cohort', 'Time', 'Symptoms', 'Support', 'Confidence', 'Lift')) 

# Supplementary Table S5 - S6: demographic/clinical characteristic, recovery clusters ------
  
  insert_msg('Supplementary Table S5 - S6, baseline of the clusters')
  
  suppl_tbl[c('baseline_clusters_north', 
              'baseline_clusters_south')] <- clust_chara$summ_tbl %>% 
    map(filter, variable %in% paper_tbl$hact_base_vars) %>% 
    map(mutate, var_r = variable) %>% 
    map(format_summ_tbl, dict = hact$dict) %>% 
    map(mutate, 
        variable = translate_var(var_r, out_value = 'label', dict = hact$dict), 
        unit = translate_var(var_r, out_value = 'unit', dict = hact$dict), 
        variable = ifelse(!is.na(unit), 
                          paste(variable, unit, sep = ', '), 
                          variable)) %>% 
    map(select, variable, starts_with('clust'), significance, eff_size) %>% 
    map(set_names, c('Variable', 
                     'Cluster #1', 'Cluster #2', 'Cluster #3', 
                     'Significance', 'Effect size'))
  
# Supplementary Table S7 - S8: acute coV and recovery, recovery clusters -----
  
  insert_msg('Supplementary Table S7 - S8, recovery in the clusters')
  
  suppl_tbl[c('cov_clusters_north', 
              'cov_clusters_south')] <- clust_chara$summ_tbl %>% 
    map(mutate, 
        var_r = variable, 
        variable = factor(variable, 
                          c(globals$cov_vars, globals$psych_var))) %>% 
    map(filter, !is.na(variable)) %>% 
    map(arrange, variable) %>% 
    map(format_summ_tbl, dict = hact$dict) %>% 
    map(mutate, 
        variable = translate_var(var_r, out_value = 'label', dict = hact$dict), 
        unit = translate_var(var_r, out_value = 'unit', dict = hact$dict), 
        variable = ifelse(!is.na(unit), 
                          paste(variable, unit, sep = ', '), 
                          variable)) %>% 
    map(select, variable, starts_with('clust'), significance, eff_size) %>% 
    map(set_names, c('Variable', 
                     'Cluster #1', 'Cluster #2', 'Cluster #3', 
                     'Significance', 'Effect size'))
  
# Saving the tables -----
  
  insert_msg('Saving the tables')
  
  paper_tbl$hact_base_vars <- NULL
  
  paper_tbl <- compact(paper_tbl)
  
  paper_tbl %>% 
    set_names(paste('Table', 1:length(paper_tbl))) %>% 
    write_xlsx(path = './paper/tables.xlsx')
  
  suppl_tbl %>% 
    set_names(paste0('Table S', 1:length(suppl_tbl))) %>% 
    write_xlsx(path = './paper/supplementary_tables.xlsx')
  
# END -----
  
  insert_tail()