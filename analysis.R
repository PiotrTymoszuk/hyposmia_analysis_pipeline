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

  insert_head()
  
  source_all('./tools/project_tools.R', 
             message = TRUE, crash = TRUE)
  
# analysis scripts ------
  
  insert_msg('Launching the analyses')
  
  c('./analysis scripts/recovery_distribution.R', 
    './analysis scripts/kinetic_modeling.R', 
    './analysis scripts/kinetic_hyposmia_long.R', 
    './analysis scripts/symptom_distances.R', 
    './analysis scripts/apriori.R', 
    './analysis scripts/pca.R', 
    './analysis scripts/clust_devel.R', 
    './analysis scripts/clustering.R', 
    './analysis scripts/clust_features.R', 
    './analysis scripts/clust_characteristic.R', 
    './analysis scripts/hyposmia_rater.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ----
  
  insert_tail()