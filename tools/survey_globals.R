# This script provides globals and import functions 

# toolbox ----

  source('./tools/sys_tools.R')

  library(readr)
  library(readxl)
  library(tibble)
  library(ggplot2)
  library(stringi)

# Co-morbidities -----
  
  globals$comorb <- c('autoimmunity', 
                      'anemia', 
                      'hypertension', 
                      'depression_burnout',
                      'diabetes', 
                      'pre_cov_epilepsy',
                      'dementia', 
                      'surgery_6m_before', 
                      'frequent_flu_like', 
                      'heart_circulation', 
                      'hay_fever', 
                      'malignancy', 
                      'pins_needles_feet', 
                      'gastrointenstinal', 
                      'lung', 
                      'two_plus_infections_antibiotics', 
                      'multiple_sclerosis', 
                      'night_dyspnoe', 
                      'bruxism', 
                      'kidney', 
                      'parkinson', 
                      'insomnia', 
                      'stroke', 
                      'embolism', 
                      'transplantation')

# Daily medication -----
  
  globals$daily_medication <- c('daily_immunosuppression', 
                                'daily_cortison', 
                                'daily_ace_drugs', 
                                'daily_pain_killers', 
                                'daily_anti_coagulation')
  
# self-reported symptoms -------
  
  globals$symptoms <- c('fever', 
                        'ague', 
                        'sore_throat', 
                        'running_nose', 
                        'fatigue', 
                        'dry_cough', 
                        'wet_cough', 
                        'breath_short', 
                        'dyspnoe', 
                        'chest_pain', 
                        'tachycardia', 
                        'extrasystole', 
                        'joint_pain', 
                        'bone_pain', 
                        'muscle_pain', 
                        'abdominal_pain', 
                        'nausea', 
                        'vomiting', 
                        'dim_appetite', 
                        'diarrhea', 
                        'dizziness', 
                        'headache', 
                        'anosmia', 
                        'taste_loss', 
                        'confusion', 
                        'tingle_feet', 
                        'tingle_hands', 
                        'ache_feet', 
                        'ache_hands', 
                        'numb_feet', 
                        'numb_hands', 
                        'unhandiness_walk', 
                        'unhandiness_micromotor', 
                        'sleep_prob', 
                        'fatigue_day', 
                        'imp_concentration', 
                        'forgetfulness', 
                        'epilepsy_covid', 
                        'swelling', 
                        'blue_fingers', 
                        'urticaria', 
                        'blister_rash', 
                        'net_rash', 
                        'red_eyes')

# colors, tiles, descriptions and labels, read from the variable lexicon ------  
  
 globals$survey_var_lexicon <- read_excel('./input data/variable_lexicon_survey.xlsx')
  
# cohort colors and labels -----
  
 globals$cohort_labels <- c(survey_at = 'Austria', 
                            survey_it = 'Italy')
 
 globals$cohort_colors <- globals$survey_var_lexicon %>% 
   filter(variable == 'cohort') %>% 
   .$level_colors %>% 
   stri_split_fixed(pattern = ', ') %>% 
   unlist %>% 
   set_names(names(globals$cohort_labels))

# END ----
