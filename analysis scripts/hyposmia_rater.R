# Sniffing stick versus self-reported hyposmia

  insert_head()
  
# container list -----
  
  rater <- list()
  
# globals: analysis tables ----
  
  insert_msg('Globals setup')
  
  ## sniffing stick score and subjective hyposmia
  ## separate datasets for the cohort and severity strata
  
  rater$analysis_tbl <- list(fup100 = sst$data_100_fup, 
                             fup360 = sst$data_360_fup) %>% 
    map(blast, cat_WHO) %>% 
    map2(., sst[c('data_100_fup', 'data_360_fup')], 
         ~c(.x, list(cohort = .y))) %>% 
    unlist(recursive = FALSE)
  
  rater$strata_labs <- 
    c('Ambulatory, CovILD study', 
      'Moderate, CovILD study', 
      'Severe, CovILD study', 
      'CovILD cohort')

# calculation of the kappas -----
  
  insert_msg('Calculation of Kohen kappas')
  
  rater$test_results <- rater$analysis_tbl %>% 
    map(correlate_variables, 
        variables = c('anosmia_sympt', 'sniff_hyposmia'), 
        what = 'correlation', 
        type = 'kappa', 
        ci = TRUE, 
        pub_styled = FALSE) %>% 
    compress(names_to = 'comparison') %>% 
    re_adjust %>% 
    mutate(estimate = ifelse(estimate < 0, 0, estimate), 
           upper_ci = ifelse(upper_ci < 0, 0, upper_ci), 
           lower_ci = ifelse(lower_ci < 0, 0, lower_ci), 
           time = stri_split_fixed(comparison, 
                                   pattern = '.', 
                                   simplify = TRUE)[, 1], 
           subset = stri_split_fixed(comparison, 
                                     pattern = '.', 
                                     simplify = TRUE)[, 2], 
           eff_size = paste0('\u03BA = ', signif(estimate, 2), 
                             ' [', signif(lower_ci, 2), ' - ', 
                             signif(upper_ci, 2), ']'), 
           plot_cap = paste(eff_size, significance, sep = ', '), 
           plot_cap = paste0(plot_cap, ', n = ', n))

# Confusion matrix stats ------
  
  insert_msg('Confusion matrix, sensitivity, specificity and co')
  
  ## rater data frame
  
  rater$rater_data <- rater$analysis_tbl %>% 
    map(~.x[c('anosmia_sympt', 'sniff_hyposmia')]) %>% 
    map(set_names, c('pred', 'obs'))
  
  ## confusion matrix
  
  rater$confusion_mtx <- rater$rater_data %>%
    map(table)
  
  ## rater stats
  
  rater$rater_stats <- rater$rater_data %>% 
    map(map_dfc, factor, c('yes', 'no')) %>% 
    map(as.data.frame)

  rater$rater_stats <- 
    list(defaultSummary, twoClassSummary) %>% 
    map(function(fun) rater$rater_stats %>% 
          map(fun, lev = c('yes', 'no'))) %>% 
    set_names(c('default', 'roc')) %>% 
    transpose %>% 
    map(reduce, c) %>% 
    reduce(rbind) %>% 
    as.data.frame %>% 
    mutate(comparison = names(rater$rater_stats), 
           roc_lab = paste0('accuracy = ', signif(Accuracy, 2), 
                           '\nsensitivity = ', signif(Sens, 2), 
                           '\nspecificity = ', signif(Spec, 2))) %>% 
    as_tibble
  
  rater$rater_stats <- 
    left_join(rater$rater_stats, 
              rater$test_results, 
              by = 'comparison')
  
# Plotting the interrater results, bubble -----
  
  insert_msg('Plotting the interrater results, bubble plots')
  
  rater$bubble_plots <- 
    list(data = rater$analysis_tbl, 
         plot_title = rep(rater$strata_labs, 2), 
         plot_subtitle = rater$test_results$plot_cap) %>% 
    pmap(plot_correlation, 
         variables = c('anosmia_sympt', 'sniff_hyposmia'), 
         scale = 'percent', 
         type = 'bubble', 
         x_lab = 'OD', 
         point_alpha = 1, 
         cust_theme = globals$common_theme) %>% 
    map(~.x + 
          labs(x = 'Self-reported OD', 
               y = 'Test OD') + 
          scale_size(limits = c(0, 71)) + 
          scale_fill_gradient2(low = 'steelblue', 
                               mid = 'white', 
                               high = 'firebrick', 
                               midpoint = 30, 
                               limits = c(0, 71)) +
          guides(fill = FALSE, 
                 size = FALSE) + 
          theme(plot.tag = element_blank()))

# Plotting confusion matrices ------
  
  insert_msg('Plotting confusion matrices')
  
  ## confusion matrices, percentages of complete observations
  ## to be plotted
  
  rater$confusion_plots$data <- rater$confusion_mtx %>% 
    map(as.data.frame) %>% 
    map(mutate, 
        percent = Freq/sum(Freq) * 100, 
        tile_lab = paste0(signif(percent, 2), '%', 
                          '\n(n = ', Freq, ')'),
        fontface = ifelse(pred == obs, 'bold', 'plain'))
  
  ## plots
  
  rater$confusion_plots$plots <- 
    list(data = rater$confusion_plots$data, 
         plot_title = rep(rater$strata_labs, 2), 
         plot_subtitle = rater$test_results$plot_cap) %>% 
    pmap(function(data, plot_title, plot_subtitle) data %>% 
           ggplot(aes(x = pred, 
                      y = obs, 
                      fill = percent)) + 
           geom_tile(color = 'black') + 
           geom_text(aes(label = tile_lab, 
                         fontface = fontface), 
                     size = 2.75) + 
           scale_fill_gradient2(low = 'steelblue', 
                                mid = 'white', 
                                high = 'firebrick', 
                                midpoint = 30, 
                                limits = c(0, 71)) +
           guides(fill = FALSE, 
                  size = FALSE) + 
           globals$common_theme +
           labs(title = plot_title, 
                subtitle = plot_subtitle, 
                x = "Self-reported OD", 
                y = "Sniffin' Stick Test OD"))
  
# ROC plots -------
  
  insert_msg('ROC plots')
  
  ## ROC plotting data: 1 denotes presence of hyposmia
  
  rater$roc$data <- rater$rater_data %>% 
    map(map_dfc, as_numeric) %>% 
    map(map_dfc, function(x) x - 1)
  
  ## ROC plot captions with the numbers of complete observations
  ## and percentages of test and subjective hyposmia
  
  rater$roc$plot_caps <- rater$confusion_plots$data %>% 
    map(~paste0('n = ', sum(.x$Freq), 
                ', test OD: ', 
                signif(sum(.x$percent[c(3, 4)]), 2), 
                '%, self-reported OD: ', 
                signif(sum(.x$percent[c(2, 4)]), 2), '%'))
  
  ## ROC plots
  
  rater$roc$plots <- 
    list(data = rater$roc$data, 
         plot_title = rep(rater$strata_labs, 2), 
         plot_subtitle = rater$roc$plot_caps, 
         line_col = rep(globals$covild_colors, 2), 
         annot_text = rater$rater_stats$roc_lab) %>% 
    pmap(function(data, plot_title, plot_subtitle, line_col, annot_text) data %>% 
           ggplot(aes(m = pred, 
                      d = obs)) + 
           geom_roc(color = line_col, 
                    labels = FALSE, 
                    cutoffs.at = 1, 
                    pointsize = 1.1) + 
           style_roc() + 
           globals$common_theme + 
           geom_abline(slope = 1, 
                       intercept = 0, 
                       linetype = 'dashed') + 
           labs(title = plot_title, 
                subtitle = plot_subtitle) + 
           annotate('text', 
                    label = annot_text, 
                    size = 2.75, 
                    color = 'black', 
                    x = 0.6, 
                    y = 0.05, 
                    hjust = 0, 
                    vjust = 0))
  
# Plotting the rates of objective and subjective hyposmia ------
  
  insert_msg('Rates of subjective and objective hyposmia')
  
  ## plotting data
  
  rater$hyposmia_rates$data <- rater$confusion_plots$data %>% 
    map(~tibble(n = sum(.x$Freq), 
                subjective = sum(.x$percent[c(2, 4)]), 
                objective = sum(.x$percent[c(3, 4)]))) %>% 
    compress(names_to = 'comparison') %>% 
    mutate(timepoint = stri_extract(comparison, regex = '\\d+'), 
           timepoint = factor(timepoint, c('100', '360')), 
           cat_WHO = stri_split_fixed(comparison, 
                                      pattern = '.', 
                                      simplify = TRUE)[, 2], 
           cat_WHO = factor(cat_WHO, 
                            c('HS', 'HM', 'A', 'cohort'))) %>% 
    blast(timepoint)
  
  ## X axis labels: severity and n numbers
  
  rater$hyposmia_rates$x_labs <- rater$hyposmia_rates$data %>% 
    map(~map2_chr(.x[['cat_WHO']], .x[['n']], 
                  paste, sep = '\nn = ')) %>% 
    map(set_names, rater$hyposmia_rates$data[[1]]$cat_WHO)
  
  ## plots for each timepoint
  
  rater$hyposmia_rates$plots <- 
    list(x = rater$hyposmia_rates$data %>% 
           map(pivot_longer, 
               cols = c('objective', 'subjective'), 
               names_to = 'type', 
               values_to = 'frequency'), 
         y = rater$hyposmia_rates$x_labs, 
         z = c('CovILD cohort, 3-month follow-up', 
               'CovILD cohort, 1-year folow-up')) %>% 
    pmap(function(x, y, z) x %>% 
           ggplot(aes(x = frequency, 
                      y = cat_WHO, 
                      fill = type)) + 
           geom_bar(stat = 'identity', 
                    color = 'black', 
                    position = position_dodge(0.9)) + 
           geom_text(aes(label = paste0(signif(frequency, 2), '%'), 
                         color = type), 
                     size = 2.75, 
                     hjust = -0.4, 
                     position = position_dodge(0.9), 
                     show.legend = FALSE) +
           scale_fill_manual(values = c(subjective = 'darkolivegreen4', 
                                        objective = 'steelblue4'), 
                             labels = c(subjective = 'self-reported', 
                                        objective = "Sniffin' Stick Test"), 
                             name = 'OD') + 
           scale_color_manual(values = c(subjective = 'darkolivegreen4', 
                                         objective = 'steelblue4'), 
                              labels = c(subjective = "self-reported", 
                                         objective = "Sniffin' Stick Test"), 
                              name = 'OD') + 
           scale_y_discrete(labels = y) + 
           scale_x_continuous(limits = c(0, 80)) + 
           globals$common_theme + 
           theme(axis.title.y = element_blank()) + 
           labs(title = z, 
                x = '% of strata'))
  
# Forest plots for Kappas ------
  
  insert_msg('Forest plots for kappas')
  
  ## plotting data
  
  rater$kappa_forests$data <- rater$test_results %>% 
    mutate(cat_WHO = factor(subset, names(globals$covild_labels))) %>% 
    blast(time)
  
  ## Forest plots
  
  rater$kappa_forests$plots <- 
    list(data = rater$kappa_forests$data, 
         plot_title = c('CovILD cohort, 3-month follow-up', 
                        'CovILD cohort, 1-year folow-up')) %>% 
    pmap(plot_forest, 
         regulation_variable = 'estimate', 
         label_variable = 'cat_WHO', 
         p_variable = 'p_adjusted', 
         lower_ci_variable = 'lower_ci', 
         upper_ci_variable = 'upper_ci', 
         cust_theme = globals$common_theme, 
         show_txt = TRUE, 
         show_ci_txt = TRUE, 
         fill_scale = c(upregulated = "steelblue4", 
                        downregulated = "steelblue4", 
                        ns = "steelblue4"), 
         x_lab = "Cohen's \u03BA, 95% CI",
         txt_hjust = -0.2) %>% 
    map(~.x + 
          scale_x_continuous(limits = c(0, 1)) + 
          scale_y_discrete(limits = c('HS', 'HM', 'A', 'cohort')) + 
          theme(legend.position = 'none'))
  
# END -----
  
  insert_tail()