# Apriori analysis of the symptoms of acute, sub-acute. long, pasc and chronic 
# COVID-19 in the HACT cohorts. Limits for the rule calculation, see code.

  insert_head()
  
# container list ----
  
  ap_sympt <- list()
  
# globals ------
  
  insert_msg('Globals: analysis tables')
  
  ## analysis tables, HACT
  
  ap_sympt$hact_tbl <- mod_tbl[c('north', 'south')] %>% 
    map(filter, time %in% c(3, 14, 28, 90)) %>% 
    map(dlply, 'time', select, -ID, -time) %>% 
    unlist(recursive = FALSE) %>% 
    map(~filter(.x, complete.cases(.x))) %>% 
    map(as.matrix)
  
  ## transaction objects
  
  ap_sympt$hact_trasactions <- ap_sympt$hact_tbl %>% 
    map(as, 'transactions')
  
# Calculating the rules -----
  
  insert_msg('Rule calculation')
  
  plan('multisession')
  
  ap_sympt$rules <- ap_sympt$hact_trasactions %>% 
    future_map(apriori, 
               parameter = list(support = 0.1, 
                                maxlen = 10, 
                                minlen = 2, 
                                confidence = 0.8), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(~.x[!is.redundant(.x)]) %>% 
    map(~subset(.x, subset = lift > 2))

# Exporting the results into a data frame, adding nicer symptom names -----
  
  insert_msg('Exporting the results to data frames')
  
  ap_sympt$rules_tbl <- ap_sympt$rules %>% 
    future_map(DATAFRAME, 
               .options = furrr_options(seed = TRUE)) %>% 
    future_map(clear_rule_tbl)
  
  plan('sequential')
  
# Plotting the confidence versus support ------
  
  insert_msg('Confidence versus support plots')
  
  ap_sympt$conf_supp_plots <- list(data = ap_sympt$rules_tbl %>% 
                                     map(mutate, 
                                         trans_lab = stri_replace(trans_lab, 
                                                                  fixed = ' \u2192 ', 
                                                                  replacement = '\u2192\n')), 
                                   plot_title = rep(c('Acute CoV', 
                                                      'Sub-acute CoV', 
                                                      'Long CoV', 
                                                      'PASC'), 2), 
                                   plot_subtitle = c(rep('AT, HACT study', 4), 
                                                     rep('IT, HACT study', 4))) %>% 
    pmap(draw_conf_supp, 
         by = 'support',
         top_transactions = 7, 
         embolden = c('{Hypo/ageusia}\u2192\n{Hypo/anosmia}', 
                      '{Hypo/anosmia}\u2192\n{Hypo/ageusia}'), 
         label_var = 'trans_lab', 
         cust_theme = globals$common_theme)
  
# Top 10 plots: support, confidence, lift -----
  
  insert_msg('Top 10 plots for the support, confidence and lift, sign. with hyposmia')
  
  ap_sympt$top_support_plots <- list(data = ap_sympt$rules_tbl %>% 
                                       map(filter, stri_detect(signature, fixed = 'anosmia')), 
                                     plot_title = rep(c('Acute CoV: top support', 
                                                        'Sub-acute CoV: top support', 
                                                        'Long CoV: top support', 
                                                        'PASC: top support'), 2), 
                                     plot_subtitle = c(rep('AT, HACT study', 4), 
                                                       rep('IT, HACT study', 4)), 
                                     fill_scale = as.list(c(rep(globals$hact_colors[1], 4), 
                                                            rep(globals$hact_colors[2], 4)))) %>% 
    pmap(plot_top, 
         regulation_variable = 'support', 
         label_variable = 'trans_lab', 
         p_variable = 'confidence', 
         signif_level = 1.1, 
         cust_theme = globals$common_theme, 
         x_lab = 'Sympt. combination support') %>% 
    map(~.x + theme(legend.position = 'none'))
  
  ap_sympt$top_confidence_plots <- list(data = ap_sympt$rules_tbl %>% 
                                          map(filter, stri_detect(signature, fixed = 'anosmia')), 
                                        plot_title = rep(c('Acute CoV: top confidence', 
                                                           'Sub-acute CoV: top confidence', 
                                                           'Long CoV: top confidence', 
                                                           'PASC: top confidence'), 2), 
                                        plot_subtitle = c(rep('AT, HACT study', 4), 
                                                          rep('IT, HACT study', 4)), 
                                        fill_scale = as.list(c(rep(globals$hact_colors[1], 4), 
                                                               rep(globals$hact_colors[2], 4)))) %>% 
    pmap(plot_top, 
         regulation_variable = 'confidence', 
         label_variable = 'trans_lab', 
         p_variable = 'confidence', 
         signif_level = 1.1, 
         cust_theme = globals$common_theme, 
         x_lab = 'Sympt. combination support') %>% 
    map(~.x + theme(legend.position = 'none'))
  
  ap_sympt$top_lift_plots <- list(data = ap_sympt$rules_tbl %>% 
                                    map(filter, stri_detect(signature, fixed = 'anosmia')), 
                                  plot_title = rep(c('Acute CoV: top lift', 
                                                     'Sub-acute CoV: top lift', 
                                                     'Long CoV: top lift', 
                                                     'PASC: top lift'), 2), 
                                  plot_subtitle = c(rep('AT, HACT study', 4), 
                                                    rep('IT, HACT study', 4)), 
                                  fill_scale = as.list(c(rep(globals$hact_colors[1], 4), 
                                                         rep(globals$hact_colors[2], 4)))) %>% 
    pmap(plot_top, 
         regulation_variable = 'lift', 
         label_variable = 'trans_lab', 
         p_variable = 'confidence', 
         signif_level = 1.1, 
         cust_theme = globals$common_theme, 
         x_lab = 'Sympt. combination support') %>% 
    map(~.x + theme(legend.position = 'none'))
  
# END -----
  
  insert_tail()