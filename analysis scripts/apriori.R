# Apriori analysis of the symptoms of acute, sub-acute. long, pasc and chronic 
# COVID-19 in the HACT cohorts. Limits for the rule calculation, see code.

  insert_head()
  
# container list ----
  
  ap_sympt <- list()
  
# Parallel backend ------
  
  insert_msg('Parallel backend')
  
  plan('multisession')
  
# globals ------
  
  insert_msg('Globals: analysis tables')
  
  ## analysis tables, HACT
  
  ap_sympt$hact_tbl <- mod_tbl[c('north', 'south')] %>% 
    map(filter, time %in% c(3, 14, 28, 90)) %>% 
    map(blast, time) %>% 
    map(map, select, -ID, -time) %>% 
    unlist(recursive = FALSE) %>% 
    map(~filter(.x, complete.cases(.x))) %>% 
    map(as.matrix)
  
  ## transaction objects
  
  ap_sympt$hact_trasactions <- ap_sympt$hact_tbl %>% 
    map(as, 'transactions')
  
# Calculating the rules -----
  
  insert_msg('Rule calculation')

  ap_sympt$rules <- ap_sympt$hact_trasactions %>% 
    future_map(apriori, 
               parameter = list(support = 0.15, 
                                maxlen = 10, 
                                minlen = 2, 
                                confidence = 0.6), 
               .options = furrr_options(seed = TRUE)) %>% 
    map(~.x[!is.redundant(.x)]) %>% 
    map(~subset(.x, subset = lift > 2))

# Exporting the results into a data frame, adding nicer symptom names -----
  
  insert_msg('Exporting the results to data frames')
  
  ap_sympt$rules_tbl <- ap_sympt$rules %>% 
    future_map(DATAFRAME, 
               .options = furrr_options(seed = TRUE)) %>% 
    future_map(clear_rule_tbl)

# Plotting the confidence versus support ------
  
  insert_msg('Confidence versus support plots')
  
  ap_sympt$conf_supp_plots <- 
    list(data = ap_sympt$rules_tbl %>% 
           map(mutate, 
               trans_lab = stri_replace(trans_lab, 
                                        fixed = ' \u2192 ', 
                                        replacement = '\u2192\n')), 
         plot_title = rep(c('0 - 14 days', 
                            '14 days', 
                            '28 days', 
                            '3 months'), 2), 
         plot_subtitle = c(rep('AT, survey study', 4), 
                           rep('IT, survey study', 4))) %>% 
    pmap(draw_conf_supp, 
         by = 'support',
         top_transactions = 7, 
         embolden = c('{Hypogeusia/ageusia}\u2192\n{OD}', 
                      '{OD}\u2192\n{Hypogeusia/ageusia}'), 
         label_var = 'trans_lab', 
         cust_theme = globals$common_theme)
  
# Top 10 plots: support, confidence, lift -----
  
  insert_msg('Top 10 plots for the support, confidence and lift, sign. with hyposmia')
  
  ap_sympt$top_support_plots <-
    list(data = ap_sympt$rules_tbl %>% 
           map(filter, stri_detect(signature, fixed = 'anosmia')), 
         plot_title = rep(c('0 - 14 days: top support', 
                            '14 days: top support', 
                            '28 days: top support', 
                            '90 days: top support'), 2), 
         plot_subtitle = c(rep('AT, survey study', 4), 
                           rep('IT, survey study', 4)), 
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
  
  ap_sympt$top_confidence_plots <- 
    list(data = ap_sympt$rules_tbl %>% 
           map(filter, stri_detect(signature, fixed = 'anosmia')), 
         plot_title = rep(c('0 - 14 days: top confidence', 
                            '14 days: top confidence', 
                            '28 days: top confidence', 
                            '90 days: top confidence'), 2), 
         plot_subtitle = c(rep('AT, survey study', 4), 
                           rep('IT, survey study', 4)), 
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
  
  ap_sympt$top_lift_plots <- 
    list(data = ap_sympt$rules_tbl %>% 
           map(filter, stri_detect(signature, fixed = 'anosmia')), 
         plot_title = rep(c('0 - 14 days: top lift', 
                            '14 days: top lift', 
                            '28 days: top lift', 
                            '90 days: top lift'), 2), 
         plot_subtitle = c(rep('AT, survey study', 4), 
                           rep('IT, survey study', 4)), 
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
  
# Plotting transactions with OD for the 28 and 90 day: bar plots -----
  
  insert_msg('Plotting bubble plots with OD transactions')
  
  ap_sympt$bubble_plots <- 
    list(data = ap_sympt$rules_tbl %>% 
           map(filter, 
               stri_detect(LHS, fixed = 'anosmia')) %>% 
           map(mutate, 
               trans_lab = stri_replace(trans_lab, 
                                        fixed = ' \u2192 ', 
                                        replacement = '\u2192\n')), 
         plot_title = rep(c('0 - 14 days', 
                            '14 days', 
                            '28 days', 
                            '3 months'), 2), 
         plot_subtitle = map2(c(rep('AT, survey study', 4), 
                                rep('IT, survey study', 4)), 
                              map_dbl(ap_sympt$hact_tbl, nrow), 
                              paste, sep = ', n = ')) %>% 
    pmap(function(data, plot_title, plot_subtitle) data %>% 
           ggplot(aes(x = support * 100, 
                      y = reorder(trans_lab, support), 
                      fill = confidence * 100, 
                      size = confidence * 100)) + 
           geom_point(stat = 'identity', 
                      color = 'black', 
                      shape = 21) + 
           geom_text(aes(label = paste0(signif(confidence * 100, 2), '%')), 
                     size = 2.75, 
                     hjust = -0.5) + 
           scale_size_continuous(breaks = seq(60, 100, by = 10), 
                                 limits = c(60, 100), 
                                 range = c(1, 5))  +
           scale_fill_viridis_c(limits = c(60, 100), 
                                breaks = seq(60, 100, by = 10), 
                                guide = 'legend', 
                                option = 'D') + 
           scale_x_continuous(limits = c(15, 40)) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = plot_title, 
                subtitle = plot_subtitle, 
                x = 'Frequency, % of cohort', 
                fill = 'Co-occurrence, %', 
                size = 'Co-occurrence, %'))
  
# END -----
  
  plan('sequential')
  
  insert_tail()