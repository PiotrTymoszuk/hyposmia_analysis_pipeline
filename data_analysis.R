# A master script executing the project analyses

  c('./tools/sys_tools.R', 
    './tools/kinetic_tools.R', 
    './tools/lm_qc_tools.R', 
    './tools/project_tools.R',
    './tools/counting_tools.R', 
    './tools/clust_tools.R') %>% 
    walk(source)
  
# Executing the analysis scripts -----
  
  insert_msg('Executing the analysis scripts')
  
  c('./analysis scripts/symptom_kinetic.R',
    './analysis scripts/hyposmia_kappa.R', 
    './analysis scripts/hyposmia_overlap.R', 
    './analysis scripts/hyposmia_uni_modeling.R', 
    './analysis scripts/time_to_recovery.R', 
    './analysis scripts/chronicity_phenotypes.R') %>% 
    walk(source)
  
# END ----
  
  insert_tail()