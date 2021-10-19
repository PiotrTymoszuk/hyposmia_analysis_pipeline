# This script checks for overlap between the sniffing-test hyposmia and the self-reported one in the COVILD cohort
# Timepoint: day 100

  insert_head()
  
# data container ----
  
  hypo_kappa <- list()
  
# globals -----
  
  insert_msg('Globals setup')

  ## analysis tables
  
  hypo_kappa$analysis_tbl$cohort <-  hyposmia$covild
  
  hypo_kappa$analysis_tbl <- hyposmia$covild %>% 
    select(ID, 
           cat_WHO, 
           anosmia_sympt_V2, ## self-declared hyposmia
           hyposmia_mod_severe_V2, ## moderate-severe test hyposmia
           hyposmia_severe) %>%  ## severe test hyposmia
    dlply(.(cat_WHO)) %>%
    c(hypo_kappa$analysis_tbl)
  
  ## variable labels
  
  hypo_kappa$var_labs <- c('hyposmia_mod_severe_V2' = 'Sniffing test < 13 pt', 
                           'hyposmia_severe' = 'Sniffing test < 8 pt', 
                           'anosmia_sympt_V2' = 'Self-reported hyposmia')

# calculating the kappas -----
  
  insert_msg('Kappa calculation')

  hypo_kappa$kappa <- c('hyposmia_mod_severe_V2', 
                        'hyposmia_severe') %>% 
    map(function(x) hypo_kappa$analysis_tbl %>% 
          map(get_kappa, 
              variable1 = 'anosmia_sympt_V2', 
              variable2 = x, 
              kappa_only = F) %>% 
          transpose) %>% 
    set_names(c('hyposmia_mod_severe_V2', 
                'hyposmia_severe'))
  
  ## summary tables
  
  hypo_kappa$summary <- hypo_kappa$kappa %>% 
    map(~.x$kappa) %>% 
    transpose %>% 
    map(reduce, rbind) %>% 
    map2_dfr(., names(.), ~mutate(.x, subset = .y)) %>% 
    mutate(p_adj = p.adjust(p_value, 'BH'), 
           kappa = ifelse(kappa < 0, 0, kappa), 
           plot_lab = paste0(signif(kappa, 2), 
                             ' [', 
                             signif(lower_ci, 2), 
                             ' - ', 
                             signif(upper_ci, 2), 
                             ']'), 
           subset_lab = globals$sev_labels[subset])
  
  ## table proportions
  
  hypo_kappa$proportions <- hypo_kappa$kappa %>% 
    map(~.x$cross_tbl) %>% 
    map(function(sympt) sympt %>% map(~.x$prop.tbl))

# n numbers -----
  
  insert_msg('Obtaining the n numbers of complate observations')
  
  hypo_kappa$n_numbers <- hypo_kappa$summary %>% 
    filter(variable2 == 'hyposmia_severe')
  
  hypo_kappa$plot_tag <- map2_chr(hypo_kappa$n_numbers$subset_lab, 
                                  hypo_kappa$n_numbers$n_number, 
                                  ~paste0(.x, ', n = ', .y))
  
# plotting the kappas as a Forest plots -----
  
  insert_msg('Plotting the kappas as Forest plots')
  
  hypo_kappa$plot <- hypo_kappa$summary %>% 
    mutate(subset_lab = factor(subset_lab, 
                               c('Cohort', 'Severe', 'Moderate', 'Ambulatory'))) %>% 
    ggplot(aes(x = kappa, 
               y = subset_lab, 
               color = subset, 
               shape = variable2)) + 
    geom_vline(xintercept = 0, 
               linetype = 'dashed') + 
    geom_errorbarh(aes(xmin = lower_ci, 
                       xmax = upper_ci), 
                   height = 0) + 
    geom_point(size = 2) +
    geom_text(aes(label = plot_lab), 
              size = 2.5, 
              hjust = 0.5, 
              vjust = -0.8) + 
    scale_color_manual(values = globals$sev_colors, 
                       labels = globals$sev_labels) + 
    scale_shape_manual(values = c('hyposmia_mod_severe_V2' = 16, 
                                  'hyposmia_severe' = 15)) + 
    guides(color = F, 
           shape = F) + 
    facet_grid(.~variable2, 
               labeller = as_labeller(hypo_kappa$var_labs)) + 
    globals$common_theme + 
    theme(axis.title.y = element_blank(), 
          panel.grid.major = element_line(color = 'gray90')) + 
    labs(title = 'Sniffing test vs. self-reported hyposmia', 
         subtitle = '100 days post COVID-19 diagnosis, COVILD cohort', 
         tag = hypo_kappa$plot_tag  %>% 
           paste(collapse = '\n') %>% 
           paste0('\n', .), 
         x = expression(kappa))
  
# plotting the cross-tables -----
  
  insert_msg('Plotting the cross tables')
  
  hypo_kappa$cross_tbl_plots <- map2(hypo_kappa$proportions, 
                                     c('Sniffing test < 13 pt', 
                                       'Sniffing test < 8 pt'), 
                                     function(x, y) list(table_object = x, 
                                                         plot_title = paste(y, globals$sev_labels[names(x)], sep = ': ')) %>% 
                                       pmap(plot_table_object, 
                                            dim_names = c('Self-reported hyposmia', 'Test hyposmia'), 
                                            plot_subtitle = '% of the subset shown in the labels', 
                                            y_lab = '% answers'))
  
# END ----
  
  insert_tail()