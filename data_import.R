# This script reads and clears the data from the Health after COVID-19 survey
# ant the COVILD-19 studies which concern hyposmia and COVID-19 course

# data containers -----

  globals <- list()
  covild_data <- list()
  survey_data <- list()
  hyposmia <- list()

# tools ------

  library(plyr)
  library(tidyverse)
  library(foreign)
  library(readxl)

  c('./tools/sys_tools.R', 
    './tools/survey_globals.R', 
    './tools/survey_import_tools.R', 
    './tools/project_tools.R') %>% 
    walk(source, 
         encoding = 'UTF-8')
  
# reading the survey and covild data -----
  
  c('./import scripts/survey.R', 
    './import scripts/covild.R') %>% 
    walk(source, 
         encoding = 'UTF-8')
  
# merging the data sets into a common list ------
  
  insert_msg('A common list with the data sets')

  hyposmia[c('covild', 
             'survey_at', 
             'survey_it')] <- c(covild_data['mod_tbl'], 
                                survey_data[c('north', 'south')])
  
  
# END -----
  
  rm(covild_data, 
     survey_data, 
     model_wrangling, 
     raw_data)
  
  insert_tail()