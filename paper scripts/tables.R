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
  
  paper_tbl$hact_baseline <- 
    as_mdtable(paper_tbl$hact_baseline, 
                                        label = 'table_1_hact_baseline', 
                                        ref_name = 'hact', 
                                        caption = 'Baseline characteristic of the Austria (AT) and Italy (IT) survey study cohorts.')
  
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
  
  paper_tbl$covild_baseline  <- as_mdtable(paper_tbl$covild_baseline, 
                                           label = 'table_2_hact_baseline', 
                                           ref_name = 'covild', 
                                           caption = 'Baseline characteristic of the CovILD study cohort and the study participants stratified by COVID-19 severity.')
  
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
  
  suppl_tbl$hact_vars <- 
    as_mdtable(suppl_tbl$hact_vars, 
               label = 'table_s1_hact_vars', 
               ref_name = 'hact_vars', 
               caption = 'Survey study variables.')
  
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
  
  suppl_tbl$covild_vars <- as_mdtable(suppl_tbl$covild_vars, 
                                      label = 'table_s2_covild_vars', 
                                      ref_name = 'covild_vars', 
                                      caption = 'CovILD study variables.')
  
# Supplementary Table S3 - S4: demographic/clinical characteristic, recovery clusters ------
  
  insert_msg('Supplementary Table S3 - S4, baseline of the clusters')
  
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
    map(select, 
        variable, all_of(names(globals$clust_colors)), 
        significance, eff_size) %>% 
    map(set_names, c('Variable', 
                     paste('Cluster', names(globals$clust_colors)), 
                     'Significance', 'Effect size'))
  
  suppl_tbl[c('baseline_clusters_north', 
              'baseline_clusters_south')] <- 
    list(x = suppl_tbl[c('baseline_clusters_north', 
                         'baseline_clusters_south')], 
         label = c('table_s3_clust_base_north', 
                   'table_s4_clust_base_south'), 
         ref_name = c('clust_base_at', 
                      'clust_base_it'), 
         caption = c('Demographic and baseline clinical characteristic at the COVID-19 onset of the survey study participants assigned to the recovery clusters, Austria (AT) cohort.', 
                     'Demographic and baseline clinical characteristic at the COVID-19 onset of the survey study participants assigned to the recovery clusters, Italy (IT) cohort.')) %>% 
    pmap(as_mdtable)
  
# Supplementary Table S5 - S6: acute coV and recovery, recovery clusters -----
  
  insert_msg('Supplementary Table S5 - S6, recovery in the clusters')
  
  suppl_tbl[c('cov_clusters_north', 
              'cov_clusters_south')] <- clust_chara$summ_tbl %>% 
    map(mutate, 
        var_r = variable, 
        variable = factor(variable, 
                          c('cov_outbreak', 
                            'weight_loss_kg', 
                            'hair_loss', 
                            'incomplete_covelescence', 
                            'perf_impairment', 
                            'new_medication_fup', 
                            'rehabilitation_fup_needed', 
                            globals$psych_var))) %>% 
    map(filter, !is.na(variable)) %>% 
    map(arrange, variable) %>% 
    map(format_summ_tbl, dict = hact$dict) %>% 
    map(mutate, 
        variable = translate_var(var_r, out_value = 'label', dict = hact$dict), 
        unit = translate_var(var_r, out_value = 'unit', dict = hact$dict), 
        variable = ifelse(!is.na(unit), 
                          paste(variable, unit, sep = ', '), 
                          variable)) %>% 
    map(select, 
        variable, all_of(names(globals$clust_colors)), 
        significance, eff_size) %>% 
    map(set_names, c('Variable', 
                     paste('Cluster', names(globals$clust_colors)), 
                     'Significance', 'Effect size'))
  
  suppl_tbl[c('cov_clusters_north', 
              'cov_clusters_south')] <- 
    list(x = suppl_tbl[c('cov_clusters_north', 
                         'cov_clusters_south')], 
         label = c('table_s5_clust_cov_north', 
                   'table_s6_clust_cov_south'), 
         ref_name = c('clust_reco_at', 
                      'clust_reco_it'), 
         caption = c('COVID-19 course and recovery in the survey study participants assigned to the recovery clusters, Austria (AT) cohort.', 
                     'COVID-19 course and recovery in the survey study participants assigned to the recovery clusters, Italy (IT) cohort.')) %>% 
    pmap(as_mdtable)
  
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