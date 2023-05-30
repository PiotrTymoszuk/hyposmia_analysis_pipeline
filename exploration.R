# Exploratory data analysis.

  library(plyr)
  library(tidyverse)
  library(exda)
  library(soucer)
  library(stringi)
  library(furrr)
  library(trafo)
  library(clustTools)
  
  insert_head()
  
  explore <- exda::explore
  
  source_all('./tools/project_tools.R')
  
# exploration scripts -----
  
  insert_msg('Launching the exploration scripts')
  
  c('./exploration scripts/cohort.R', 
    './exploration scripts/symptoms.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
  ## resorting to cahced data of the sample size analysis
  
  if(file.exists('./cache/eda_size.RData')) {
    
    insert_msg('Loading cached sample size calculation results')
    
    load('./cache/eda_size.RData')
    
  } else {
    
    source_all('./exploration scripts/power.R', 
               message = TRUE, crash = TRUE)
    
    
  }
  
# END ------
  
  insert_tail()