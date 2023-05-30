# Launches particular analysis scripts

  library(plyr)
  library(tidyverse)
  library(exda)
  library(soucer)
  library(stringi)
  library(kinet)
  library(furrr)
  library(clustTools)
  library(somKernels)
  library(ggrepel)
  library(dbscan)
  library(arules)
  library(arulesViz)
  library(microViz)
  library(trafo)
  library(rlang)
  library(caret)
  library(plotROC)
  library(rstatix)
  library(rcompanion)
  library(ggalluvial)

  insert_head()
  
  explore <- exda::explore
  
  source_all('./tools/project_tools.R', 
             message = TRUE, crash = TRUE)
  
# analysis scripts ------
  
  insert_msg('Launching the analyses')
  
  c('./analysis scripts/recovery_distribution.R', 
    './analysis scripts/kinetic_modeling.R', 
    './analysis scripts/kinetic_hyposmia_long.R', 
    './analysis scripts/symptom_distances.R', 
    './analysis scripts/apriori.R', 
    './analysis scripts/pca.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  ## fetching cluster development results from the cache
  
  if(file.exists('./cache/cl_devel.RData')) {
    
    insert_msg('Loading cached cluster development results')
    
    load('./cache/cl_devel.RData')
    
  } else {
    
    source_all('./analysis scripts/clust_devel.R', 
               message = TRUE, crash = TRUE)
    
  }
  
  source_all('./analysis scripts/clustering.R', 
             message = TRUE, crash = TRUE)
  
  ## clustering factor importance, cached
  
  if(file.exists('./cache/clust_imp.RData')) {
    
    insert_msg('Loading cached clustering importance results')
    
    load('./cache/clust_imp.RData')
    
  } else {
    
    source_all('./analysis scripts/clustering_importance.R', 
               message = TRUE, crash = TRUE)
    
  }
  
  c('./analysis scripts/clust_features.R', 
    './analysis scripts/clust_characteristic.R', 
    './analysis scripts/hyposmia_rater.R', 
    './analysis scripts/sst_kinetic.R', 
    './analysis scripts/od_kinetic.R') %>% 
    source_all(message = TRUE, crash = TRUE)

# END ----
  
  insert_tail()