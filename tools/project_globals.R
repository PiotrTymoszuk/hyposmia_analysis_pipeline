# This script contains project globals

# data container ------

  globals <- list()

# graphics -----

  globals$common_text <- element_text(size = 8,
                                      face = 'plain',
                                      color = 'black')

  globals$common_margin <- ggplot2::margin(t = 5,
                                           l = 4,
                                           r = 2,
                                           unit = 'mm')

  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text,
                                                  axis.title = globals$common_text,
                                                  plot.title = element_text(size = 8,
                                                                            face = 'bold'),
                                                  plot.subtitle = globals$common_text,
                                                  plot.tag = element_text(size = 8,
                                                                          face = 'plain',
                                                                          color = 'black',
                                                                          hjust = 0,
                                                                          vjust = 1),
                                                  plot.tag.position = 'bottom',
                                                  legend.text = globals$common_text,
                                                  legend.title = globals$common_text,
                                                  strip.text = globals$common_text,
                                                  strip.background = element_rect(fill = 'gray95',
                                                                                  color = 'gray80'),
                                                  plot.margin = globals$common_margin,
                                                  panel.grid.major = element_line(color = 'gray90'))
  
# strata colors ------
  
  globals$hact_colors <- c(north = 'steelblue', 
                           south = 'coral3')
  
  globals$hact_labs <- c(north = 'AT', 
                         south = 'IT')
  
  globals$covild_colors <- c(A = 'steelblue', 
                             HM = 'darkolivegreen3', 
                             HS = 'coral3', 
                             cohort = 'plum4')
  
  globals$covild_labels <- c(A = 'ambulatory', 
                             HM = 'moderate', 
                             HS = 'severe', 
                             cohort = 'CovILD cohort')
  
# symptom variables ------
  
  globals$hact_symptoms <- c('fever', 'ague', 'sore_throat', 'running_nose', 
                             'fatigue', 'dry_cough', 'wet_cough', 'breath_short', 
                             'dyspnoe', 'chest_pain', 'tachycardia', 'extrasystole', 
                             'joint_pain', 'bone_pain', 'muscle_pain', 'abdominal_pain', 
                             'nausea', 'vomiting', 'dim_appetite', 'diarrhea', 
                             'dizziness', 'anosmia', 'taste_loss', 'confusion', 
                             'tingle_feet', 'tingle_hands', 'ache_feet', 'ache_hands', 
                             'numb_feet', 'numb_hands', 'unhandiness_walk', 'unhandiness_micromotor', 
                             'sleep_prob', 'fatigue_day', 'imp_concentration', 'forgetfulness', 
                             'swelling', 'blue_fingers', 'urticaria', 
                             'blister_rash', 'net_rash', 'red_eyes')
  
  globals$hact_sympt_class <- tibble(variable = globals$hact_symptoms, 
                                     class = c(fever = 'infection', 
                                               ague = 'infection', 
                                               sore_throat = 'infection', 
                                               running_nose = 'infection', 
                                               fatigue = 'performance/sleep', 
                                               dry_cough = 'respiratory', 
                                               wet_cough = 'respiratory', 
                                               breath_short = 'respiratory', 
                                               dyspnoe = 'respiratory', 
                                               chest_pain = 'pain', 
                                               tachycardia = 'heart', 
                                               extrasystole = 'heart', 
                                               joint_pain = 'pain', 
                                               bone_pain = 'pain',
                                               muscle_pain = 'pain', 
                                               abdominal_pain = 'pain', 
                                               nausea = 'gastrointestinal', 
                                               vomiting = 'gastrointestinal', 
                                               dim_appetite = 'gastrointestinal', 
                                               diarrhea = 'gastrointestinal', 
                                               dizziness = 'neurocognitive', 
                                               anosmia = 'smell/taste', 
                                               taste_loss = 'smell/taste', 
                                               confusion = 'neurocognitive', 
                                               tingle_feet = 'neurological', 
                                               tingle_hands = 'neurological', 
                                               ache_feet = 'neurological', 
                                               ache_hands = 'neurological', 
                                               numb_feet = 'neurological',
                                               numb_hands = 'neurological', 
                                               unhandiness_walk = 'neurological', 
                                               unhandiness_micromotor = 'neurological', 
                                               sleep_prob = 'performance/sleep', 
                                               fatigue_day = 'performance/sleep', 
                                               imp_concentration = 'neurocognitive', 
                                               forgetfulness = 'neurocognitive', 
                                               swelling = 'skin/eyes', 
                                               blue_fingers = 'skin/eyes', 
                                               urticaria = 'skin/eyes', 
                                               blister_rash = 'skin/eyes', 
                                               net_rash = 'skin/eyes', 
                                               red_eyes = 'skin/eyes')) %>% 
    mutate(class = factor(class, c('infection',
                                   'respiratory', 
                                   'pain', 
                                   'gastrointestinal', 
                                   'heart', 
                                   'skin/eyes', 
                                   'smell/taste', 
                                   'performance/sleep', 
                                   'neurocognitive', 
                                   'neurological')))
  
  globals$hact_symptom_order <- globals$hact_sympt_class %>% 
    arrange(class, variable) %>% 
    .$variable

  globals$covild_symptoms <- c('sleep_sympt', 
                               'dyspnoe_sympt', 
                               'cough_sympt', 
                               'fever_sympt', 
                               'night_sweat_sympt', 
                               'gastro_sympt', 
                               'anosmia_sympt', 
                               'fatigue_sympt')
  
# Clusters -----
  
  globals$clust_colors <- c('STDR' = 'darkorange3', 
                            'RR' = 'darkolivegreen3', 
                            'SR' = 'cadetblue3')
  
# demographic and clinical variables,  HACS -----
  
  globals$demo_vars <- c('sex', 'age', 'bmi_class_before', 
                         'cohabitants', 'household_size', 
                         'education_class', 'employment_before', 
                         'employment_sector', 'obs_time')
  
  globals$clinic_vars <- c('smoking', 
                           'comorb_present', 'comorb_sum', 
                           'hypertension', 'heart_circulation', 'diabetes', 
                           'lung', 'gastrointenstinal', 'malignancy', 
                           'hay_fever', 'autoimmunity', 
                           'frequent_flu_like', 'two_plus_infections_antibiotics', 
                           'depression_burnout', 'insomnia', 'night_dyspnoe', 
                           'bruxism', 'pins_needles_feet', 
                           'daily_medication')
  
  globals$cov_vars <- c('cov_outbreak', 
                        'illness_feeling', 
                        'sum_symptoms_acute', 
                        'sum_symptoms_long', 
                        'weight_loss_kg', 
                        'hair_loss', 
                        'incomplete_covelescence', 
                        'perf_impairment', 
                        'new_medication_fup', 
                        'rehabilitation_fup_needed')
  
  globals$psych_var <- c('phq_anxiety_score', 
                         'phq_depression_score', 
                         'stress_score', 
                         'mental_health_score', 
                         'life_quality_score')
  
# demographic and clinical variables, CovILD ----
  
  globals$covild_vars <- c('sex', 'age', 'weight_class', 
                           'comorb_present', 'no_comorb', 
                           'endometabolic_comorb', 'hypertension_comorb', 
                           'cardiovascular_comorb', 'diabetes_comorb', 
                           'pulmonary_comorb', 'gastro_comorb', 
                           'malingancy_comorb', 'immdef_comorb', 
                           'cat_WHO')

# END -----
