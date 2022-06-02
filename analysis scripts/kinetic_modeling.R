# Modeling of the recovery kinetic for particular symptoms: top 10 most common
# symptoms of acute COVID-19 in the  HACT study and for all symptoms in the 
# CovILD study.
# Second order model with the participant random effect.
# Only participants with the complete answer sets are included in the analysis.

  insert_head()
  
# container list ----
  
  kin_mod <- list()
  
# globals ------
  
  insert_msg('Globals')
  
  kin_mod$covild_tbl <- mod_tbl$covild %>% 
    complete_cases %>% 
    dlply('cat_WHO', select, -cat_WHO) %>% 
    map(as_tibble)
  
  kin_mod$hact_top_symptoms <- eda_sympt$hact_top$acute
  
# model construction for the HACST study ------
  
  insert_msg('Building the models, HACT')
  
  plan('multisession')

  kin_mod$models_hact$north <- kin_mod$hact_top_symptoms %>% 
    future_map(~safely(model_kinetic)(data = mod_tbl$north[c('ID', 'time', .x)] %>% 
                                        complete_cases, 
                                      response = .x, 
                                      time = 'time', 
                                      ID = 'ID', 
                                      family = 'binomial', 
                                      order = 2), 
               .options = furrr_options(seed = TRUE))
  
  kin_mod$models_hact$south <- kin_mod$hact_top_symptoms %>% 
    future_map(~safely(model_kinetic)(data = mod_tbl$south[c('ID', 'time', .x)] %>% 
                                        complete_cases, 
                                      response = .x, 
                                      time = 'time', 
                                      ID = 'ID', 
                                      family = 'binomial', 
                                      order = 2), 
               .options = furrr_options(seed = TRUE))
  
  kin_mod$models_hact <- kin_mod$models_hact %>% 
    map(~map(.x, ~.x$result)) %>% 
    map(set_names, kin_mod$hact_top_symptoms) %>% 
    map(compact)
  
# Builing the models for the CovILD study ------
  
  insert_msg('Building the models, CovILD')
  
  kin_mod$models_covild$A <- globals$covild_symptoms %>% 
    future_map(~safely(model_kinetic)(data = kin_mod$covild_tbl$A[c('ID', 'time_numeric', .x)] %>% 
                                        complete_cases, 
                                      response = .x, 
                                      time = 'time_numeric', 
                                      ID = 'ID', 
                                      family = 'binomial', 
                                      order = 2), 
               .options = furrr_options(seed = TRUE))
  
  kin_mod$models_covild$HM <- globals$covild_symptoms %>% 
    future_map(~safely(model_kinetic)(data = kin_mod$covild_tbl$HM[c('ID', 'time_numeric', .x)] %>% 
                                        complete_cases, 
                                      response = .x, 
                                      time = 'time_numeric', 
                                      ID = 'ID', 
                                      family = 'binomial', 
                                      order = 2), 
               .options = furrr_options(seed = TRUE))
  
  kin_mod$models_covild$HS <- globals$covild_symptoms %>% 
    future_map(~safely(model_kinetic)(data = kin_mod$covild_tbl$HS[c('ID', 'time_numeric', .x)] %>% 
                                        complete_cases, 
                                      response = .x, 
                                      time = 'time_numeric', 
                                      ID = 'ID', 
                                      family = 'binomial', 
                                      order = 2), 
               .options = furrr_options(seed = TRUE))
  
  kin_mod$models_covild <- kin_mod$models_covild %>% 
    map(~map(.x, ~.x$result)) %>% 
    map(set_names, globals$covild_symptoms) %>% 
    map(compact)
  
  plan('sequential')
  
# LRT testing -----
  
  insert_msg('LRT')
  
  kin_mod$lrt_hact <- kin_mod$models_hact %>% 
    map(lrt_list, .parallel = TRUE)
  
  kin_mod$lrt_covild <- kin_mod$models_covild %>% 
    map(lrt_list, .parallel = FALSE)
  
  ## summary tables plot captions
  
  kin_mod$lrt_summ_hact <- kin_mod$lrt_hact %>% 
    map(~map_dfr(.x, filter, order == 'global')) %>% 
    map(mutate, p_value = Pr..Chisq.) %>% 
    map(re_adjust) %>% 
    map(mutate, 
        eff_size = paste('\u03BB =', signif(lambda, 2)), 
        plot_cap = paste(eff_size, significance, sep = ', '))
  
  kin_mod$lrt_summ_covild <- kin_mod$lrt_covild %>% 
    map(~map_dfr(.x, filter, order == 'global')) %>% 
    map(mutate, p_value = Pr..Chisq.) %>% 
    map(re_adjust) %>% 
    map(mutate, 
        eff_size = paste('\u03BB =', signif(lambda, 2)), 
        plot_cap = paste(eff_size, significance, sep = ', '))

# Plotting, HACT ------
  
  insert_msg('Recovery kinetic plots, HACT')

  kin_mod$plots_hact <- list(mod = kin_mod$models_hact, 
                             sub = c('AT, HACT study', 'IT, HACT study'), 
                             col = globals$hact_colors) %>% 
    pmap(function(mod, sub, col) list(x = mod, 
                                      plot_title = translate_var(names(mod), 
                                                                 dict = hact$dict)) %>% 
           pmap(plot, 
                type = 'frequency', 
                plot_subtitle = sub, 
                x_lab = 'Days post CoV', 
                cust_theme = globals$common_theme, 
                point_color = col,
                outcome_color = col, 
                label_size = 2.75) %>% 
           map(~.x + 
                 theme(legend.position = 'none') + 
                 scale_y_continuous(limits = c(0, 100)) + 
                 scale_x_continuous(limits = c(0, 105), 
                                    breaks = c(0, 14, 28, 90))))
  
# Plotting, CovILD ------
  
  insert_msg('Recovery kinetic plots, CovILD')

  kin_mod$plots_covild <- list(mod = kin_mod$models_covild, 
                             sub = c('Ambulatory, CovILD study', 
                                     'Moderate, CovILD study', 
                                     'Severe, CovILD study'), 
                             col = globals$covild_colors[1:3]) %>% 
    pmap(function(mod, sub, col) list(x = mod, 
                                      plot_title = translate_var(names(mod), 
                                                                 dict = covild$dict)) %>% 
           pmap(plot, 
                type = 'frequency', 
                plot_subtitle = sub, 
                x_lab = 'Days post CoV', 
                cust_theme = globals$common_theme, 
                point_color = col,
                outcome_color = col, 
                label_size = 2.75) %>% 
           map(~.x + 
                 theme(legend.position = 'none') + 
                 scale_y_continuous(limits = c(0, 105), 
                                    breaks = seq(0, 100, by = 25)) + 
                 scale_x_continuous(limits = c(0, 370), 
                                    breaks = c(0, 60, 100, 180, 360))))
  
  
# END ----
  
  insert_tail()