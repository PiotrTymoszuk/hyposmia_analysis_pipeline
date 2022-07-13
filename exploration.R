# Exploratory data analysis.

  library(plyr)
  library(tidyverse)
  library(exda)
  library(soucer)
  library(stringi)
  library(furrr)
  
  insert_head()
  
  source_all('./tools/project_tools.R')
  
# exploration scripts -----
  
  insert_msg('Launching the exploration scripts')
  
  c('./exploration scripts/cohort.R', 
    './exploration scripts/symptoms.R') %>% 
    source_all(message = TRUE, crash = TRUE)
  
# END ------
  
  insert_tail()