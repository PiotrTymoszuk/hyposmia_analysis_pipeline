# Modeling of the recovery kinetic for particular symptoms: top 10 most common
# symptoms of acute COVID-19 in the  HACT study and for all symptoms in the 
# CovILD study.
# Second order model with the participant random effect.
# Only participants with the complete answer sets are included in the analysis.
#
# An alternative testing approach: Cochran's Q test with the Kendall's W 
# effect size defined as w or eta^2 = Q/b(k - 1)
  insert_head()
  
# container list ----
  
  kin_mod <- list()
  
# globals ------
  
  insert_msg('Globals')
  
  ## symptom variables: top most frequwnt symptoms for HACT
  ## all recorded symptoms for CovILD
  
  kin_mod$variables <- 
    list(hact = eda_sympt$hact_top$acute, 
         covild = globals$covild_symptoms)

  ## analysis tables with complete longitudinal record
  
  kin_mod$hact_tbl <- mod_tbl[c("north", "south")] %>% 
    map(relocate, ID) %>% 
    map(complete_cases)
  
  kin_mod$covild_tbl <- mod_tbl$covild %>% 
    #complete_cases %>% 
    blast(cat_WHO) %>% 
    map(select, -cat_WHO) %>% 
    map(as_tibble)
  
# model construction for the HACT study ------
  
  insert_msg('Building the models, HACT')
  
  plan('multisession')
  
  for(i in names(kin_mod$hact_tbl)) {
    
    kin_mod$models_hact[[i]] <- kin_mod$variables$hact %>% 
      future_map(~safely(model_kinetic)(data = kin_mod$hact_tbl[[i]], 
                                        response = .x, 
                                        time = 'time', 
                                        ID = 'ID', 
                                        family = 'binomial', 
                                        order = 2), 
                 .options = furrr_options(seed = TRUE))
    
  }

  kin_mod$models_hact <- kin_mod$models_hact %>% 
    map(~map(.x, ~.x$result)) %>% 
    map(set_names, kin_mod$variables$hact) %>% 
    map(compact)
  
# Building the models for the CovILD study ------
  
  insert_msg('Building the models, CovILD')
  
  for(i in names(kin_mod$covild_tbl)) {
    
    kin_mod$models_covild[[i]] <- kin_mod$variables$covild %>% 
      future_map(~safely(model_kinetic)(data = kin_mod$covild_tbl[[i]][c('ID', 'time_numeric', .x)] %>% 
                                          complete_cases, 
                                        response = .x, 
                                        time = 'time_numeric', 
                                        ID = 'ID', 
                                        family = 'binomial', 
                                        order = 2), 
                 .options = furrr_options(seed = TRUE))
    
  }
  
  kin_mod$models_covild <- kin_mod$models_covild %>% 
    map(~map(.x, ~.x$result)) %>% 
    map(set_names, kin_mod$variables$covild) %>% 
    map(compact)
  
  plan('sequential')
  
# LRT testing -----
  
  insert_msg('LRT')
  
  ## Cochran's Q test user instead!!!
  
 # kin_mod$lrt_hact <- kin_mod$models_hact %>% 
  #  map(lrt_list, .parallel = TRUE)
  
#  kin_mod$lrt_covild <- kin_mod$models_covild %>% 
 #   map(lrt_list, .parallel = FALSE)
  
  ## summary tables plot captions
  
#  kin_mod[c('lrt_summ_hact', 
 #           'lrt_summ_covild')] <- 
  #  kin_mod[c('lrt_hact', 'lrt_covild')] %>% 
   # map(function(lrt) lrt %>% 
    #      map(~map_dfr(.x, filter, order == 'global')) %>% 
     #     map(mutate, p_value = Pr..Chisq.) %>% 
      #    map(re_adjust) %>% 
       #   map(mutate, 
        #      eff_size = paste('\u03BB =', signif(lambda, 2)), 
         #     plot_cap = paste(eff_size, significance, sep = ', ')))

# Cochran's Q test -------
  
  insert_msg('Cochran Q test')
  
  kin_mod$cochran_formulas <- kin_mod$variables %>% 
    map(~set_names(.x, .x)) %>% 
    map(~map(.x, ~paste(.x, '~ time | ID'))) %>% 
    map(map, formula)

  ## testing, HACT
  
  kin_mod$test_hact <- kin_mod$hact_tbl %>% 
    map(mutate, 
        time = factor(time)) %>% 
    map(function(data) kin_mod$cochran_formulas$hact %>% 
          map_dfr(~cochran_qtest(data = data, formula = .x))) %>% 
    map(re_adjust, p_variable = 'p') %>% 
    map2(., kin_mod$hact_tbl, 
         ~mutate(.x, 
                 w = statistic/(n * (length(levels(factor(.y$time))) - 1)), 
                 eff_size = paste('W =', signif(w, 2))))
  
  ## testing, CovILD
  
  kin_mod$test_covild <- kin_mod$covild_tbl %>% 
    map(mutate, 
        time = factor(time_numeric)) %>% 
    map(function(data) kin_mod$cochran_formulas$covild %>% 
          map2_dfr(., names(.), 
                   ~cochran_qtest(data = data[c('ID', 'time', .y)] %>% 
                                    complete_cases, 
                                  formula = .x))) %>% 
    map(re_adjust, p_variable = 'p') %>% 
    map2(., kin_mod$covild_tbl, 
         ~mutate(.x, 
                 w = statistic/(n * (length(levels(factor(.y$time_numeric))) - 1)), 
                 eff_size = paste('W =', signif(w, 2))))
  
# Plotting, HACT ------
  
  insert_msg('Recovery kinetic plots, HACT')

  kin_mod$plots_hact <- list(mod = kin_mod$models_hact, 
                             sub = c('AT, survey study', 'IT, survey study'), 
                             col = globals$hact_colors) %>% 
    pmap(function(mod, sub, col) list(x = mod, 
                                      plot_title = exchange(names(mod), 
                                                            dict = hact$dict)) %>% 
           pmap(plot, 
                type = 'frequency', 
                plot_subtitle = sub, 
                x_lab = 'Days post CoV', 
                cust_theme = globals$common_theme, 
                point_color = col,
                outcome_color = col, 
                label_size = 2.5) %>% 
           map(~.x + 
                 theme(legend.position = 'none') + 
                 scale_y_continuous(limits = c(0, 105)) + 
                 scale_x_continuous(limits = c(0, 105), 
                                    breaks = c(0, 14, 28, 90))))
  
  ## moving n numbers to the captions
  
  kin_mod$plots_hact <- kin_mod$plots_hact %>% 
    map(~map(.x, tag_to_sub))
  
  
# Plotting, CovILD ------
  
  insert_msg('Recovery kinetic plots, CovILD')

  kin_mod$plots_covild <- list(mod = kin_mod$models_covild, 
                               sub = c('Ambulatory, CovILD study', 
                                       'Moderate, CovILD study', 
                                       'Severe, CovILD study'), 
                               col = globals$covild_colors[1:3]) %>% 
    pmap(function(mod, sub, col) list(x = mod, 
                                      plot_title = exchange(names(mod), 
                                                            dict = covild$dict)) %>% 
           pmap(plot, 
                type = 'frequency', 
                plot_subtitle = sub, 
                x_lab = 'Days post CoV', 
                cust_theme = globals$common_theme, 
                point_color = col,
                outcome_color = col, 
                label_size = 2.5) %>% 
           map(~.x + 
                 theme(legend.position = 'none') + 
                 scale_y_continuous(limits = c(0, 105), 
                                    breaks = seq(0, 100, by = 25)) + 
                 scale_x_continuous(limits = c(0, 370), 
                                    breaks = c(0, 60, 100, 180, 360))))
  
  ## moving n numbers to the captions
  
  kin_mod$plots_covild <- kin_mod$plots_covild %>% 
    map(~map(.x, tag_to_sub))

# END ----

  rm(i)
  
  insert_tail()