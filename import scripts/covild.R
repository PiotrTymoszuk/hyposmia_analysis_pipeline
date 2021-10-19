# This script imports the data of the CovILD study


  insert_head()
  
# Data containers ----
  
  prev_visits <- list()
  neuro_data <- list()
  qol_data <- list()
  
  covild_data <- list()

# Globals ----
  
  insert_msg('Globals setup')

  ## graphics
  
  globals$corr_colors <- c('negative' = 'steelblue4', 
                           'positive' = 'firebrick4', 
                           'ns' = 'gray60')
  
  globals$common_text <- element_text(size = 8, face = 'plain', color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 4, l = 3, r = 2, unit = 'mm')
  
  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text, 
                                                  axis.title = globals$common_text, 
                                                  plot.title = element_text(size = 8, 
                                                                            face = 'bold', 
                                                                            color = 'black', 
                                                                            hjust = 0), 
                                                  plot.subtitle = globals$common_text, 
                                                  plot.tag = element_text(size = 8, 
                                                                          face = 'plain', 
                                                                          color = 'black', 
                                                                          hjust = 0), 
                                                  plot.tag.position = 'bottom', 
                                                  legend.text = globals$common_text, 
                                                  legend.title = globals$common_text, 
                                                  strip.text = globals$common_text,
                                                  strip.background = element_rect(fill = 'gray95', color = 'gray80'), 
                                                  plot.margin = globals$common_margin)
  
  ## patient severity group colors and labels
  
  globals$sev_colors <- c(A = 'steelblue', 
                          HM = 'indianred2', 
                          HS = 'firebrick4', 
                          cohort = 'coral3')
  
  globals$sev_labels <- c(A = 'Ambulatory', 
                          HM = 'Moderate', 
                          HS = 'Severe', 
                          cohort = 'Cohort')

# reading the recoding scheme -----
  
  insert_msg('Reading the recoding scheme')
  
  globals$var_coding[c('prev_visits', 
                       'neuro_covild', 
                       'qol')] <- c('prev_visits', 
                                    'neuro_covild', 
                                    'qol') %>% 
    map(read_excel, 
        path = './input data/var_recoding_covild.xlsx') %>% 
    map(mutate, 
        args1 = stri_replace_all(args1, regex = '“|”', replacement = ''), 
        args1 = stri_replace_all(args1, regex = '‘|’', replacement = "'"), 
        args1 = map(args1, function(x) if(any(is.na(x))) NULL else x)) %>% 
    map(mutate, 
        args2 = stri_replace_all(args2, regex = '“|”', replacement = ''), 
        args2 = stri_replace_all(args2, regex = '‘|’', replacement = "'"), 
        args2 = map(args2, function(x) if(any(is.na(x))) NULL else x)) %>% 
    map(mutate, 
        args = map2(args1, args2, list)) %>% 
    map(select, 
        - args1, 
        - args2)

# reading the variable lexicon -----
  
  insert_msg('Reading the variable lexicon')
  
  globals$var_lexicon <- read_excel('./input data/var_lexicon_covild.xlsx') %>% 
    mutate(new_name = ifelse(is.na(time_lab), 
                             variable, 
                             paste(variable, time_lab, sep = '_')))

# setup of the modeling variables -----
  
  insert_msg('Modeling variables')
  
  globals[c('responses', 
            'variables')] <- c('response', 
                               'independent') %>% 
    map(function(x) filter(globals$var_lexicon, var_type == x))
  
# data till the 3 month visit -----
  
  insert_msg('Reading and cleaning the data from the previous visits')
  
  ## reading the excel data
  
  prev_visits$prev_visits <- read_excel('./input data/Covid_V0bisV2_tc_271220.xlsx')
  
  ## new patient classification
  
  prev_visits$sev_class <- read_excel('./input data/CovILD V3.xlsx', sheet = 'demographics') %>% 
    select(ID, WHO, cat_WHO) %>% 
    mutate(cat_WHO = car::recode(cat_WHO, 
                                 "'hospitalized,mild' = 'HM'; 
                                 'ambulatory' = 'A'; 
                                 'hospitalized, severe' = 'HS'"))
  
  prev_visits$prev_visits <- left_join(prev_visits$prev_visits, 
                                       prev_visits$sev_class, 
                                       by = 'ID')
  
  ## antibody level stratification
  
  prev_visits$ab_quant <- cut(prev_visits$prev_visits$SarsCov2_IgG, 
                             quantile(prev_visits$prev_visits$SarsCov2_IgG, 
                                      c(0, 0.25, 0.5, 0.75, 1), 
                                      na.rm = T), 
                             c('Q1', 'Q2', 'Q3', 'Q4'), 
                             include.lowest = T)
  
  ## manual re-coding
  
  prev_visits$prev_visits <- prev_visits$prev_visits %>% 
    mutate(smoking = ifelse(current_smoker == 1,
                            'active', 
                            ifelse(smoking_ex == 0, 
                                   'never', 'ex')), 
           TSAT_red = ifelse((gender == 0 & TSAT < 15)|(gender == 1 & TSAT < 20), 
                             'yes', 'no'), 
           anemia = ifelse((gender == 0 & Hb < 120)|(gender == 1 & Hb < 140), 
                           'yes', 'no'), 
           FT_elv = ifelse((gender == 0 & ferritin >= 150)|(gender == 1 & ferritin >= 300), 
                           'yes', 'no'), 
           sympt_number = sleep_disorder + dyspnoe + cough + fever + night_sweat + GI_sympt  + anosmia + ECOG_imp  + pain, 
           sympt_present = ifelse(time == 0, 
                                  ifelse(sympt_number > 0, 
                                         'yes', 
                                         'no'), 
                                  recode_yn(persistent_symptoms, F, F)), 
           ab_quant = prev_visits$ab_quant, 
           age_class = cut(age, c(-Inf, 50, 65, Inf), c('up to 50', '51 - 65', 'over 65')))
  
  ## automated cleaning
  
  prev_visits <- globals$var_coding$prev_visits %>% 
    pmap(recode_var, 
         data = prev_visits$prev_visits, 
         time_var = 'time') %>% 
    reduce(left_join, 
         by = c('ID', 'time')) %>% 
    filter(time %in% c(0, 1, 2, 3))
  
# 3 month Neuro CovILD data ------
  
  insert_msg('Reading and cleaning the data from the 100-day/V3 neuro visit')
  
  neuro_data$neuro_data <-  read.spss('./input data/COVIDPat_135_cluster.sav', 
                                      to.data.frame = T) %>% 
    as_tibble %>% 
    mutate(time = 4,  
           ID = stri_replace(pat_id, regex = '\\s+', replacement = ''), 
           sleep_disord_new = ifelse(Sleep_disturbance_3m %in% c('keine', 'alt'),
                                     'no', 'yes') %>% 
             factor, 
           sleep_disord_precov = ifelse(Sleep_disturbance_3m == 'alt',
                                        'yes', 'no') %>% 
             factor, 
           hypogeusia_self = ifelse(gschmackssinn != 'subjektiv normal', ## taste change or loss, self-reported
                                    'yes', 'no') %>% 
             factor, 
           cephalea_precov = ifelse(Cephalea_3m_old != 0, 'yes', 'no') %>%  ## headache before COVID-19
             factor)
  
  neuro_data <- globals$var_coding$neuro_covild %>% 
    pmap(recode_var, 
         data = neuro_data$neuro_data, 
         time_var = NULL) %>% 
    reduce(left_join, 
           by = 'ID')
  
# 3 month QoL data -----
  
  insert_msg('Reading and clearing the 3 month QoL data')
  
  qol_data$qol_data <-  read.spss('./input data/COVIDPat_SF36.sav', 
                                  to.data.frame = T) %>% 
    as_tibble %>% 
    mutate(time = 5, 
           ID = stri_replace(pat_id, regex = '\\s+', replacement = ''))
  
  qol_data <- globals$var_coding$qol %>% 
    pmap(recode_var, 
         data = qol_data$qol_data, 
         time_var = NULL) %>% 
    reduce(left_join, 
           by = 'ID')
  
# merging and generation of the fixed-time modeling table ------
  
  insert_msg('Merging the datasets, constant setup')
  
  covild_data$data_tbl <- list(prev_visits, 
                               neuro_data, 
                               qol_data) %>% 
    reduce(outer_rbind)

  constants <- globals$var_lexicon %>% 
    filter(!is.na(reference))

  covild_data$mod_tbl <- 1:nrow(constants) %>% 
    map(function(x) set_constant(data = covild_data$data_tbl, 
                                 variable = constants$variable[x], 
                                 reference = constants$reference[x], 
                                 new_name = constants$new_name[x], 
                                 ID_var = 'ID', 
                                 time_var = 'time') %>% 
          select(constants$new_name[x], 
                 ID, 
                 time)) %>% 
    reduce(left_join, 
           by = c('ID', 'time'))

  covild_data$mod_tbl <- covild_data$mod_tbl %>% 
    filter(time == 0) %>% 
    map_dfc(function(x) if(is.character(x)) factor(x) else x) %>% 
    mutate(ID = as.character(ID), 
           cat_WHO = factor(cat_WHO, levels = c('A', 'HM', 'HS')), 
           TSAT_red_V1 = factor(TSAT_red_V1, c('no', 'yes')), 
           RV_red_V1 = factor(RV_red_V1, c('no', 'yes')))

# END -----
  
  rm(constants, neuro_data, prev_visits, qol_data)

  insert_tail()