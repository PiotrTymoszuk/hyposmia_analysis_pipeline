# Kinetic of subjective and objective hyposmia in the CovILD cohort
# Paired proportions at the 3- and 12-month follow-up  are compared with 
# McNemar test and Cohen's G effect size statistic 

  insert_head()
  
# container ------
  
  od_kinet <- list()
  
# analysis globals ------
  
  insert_msg('Analysis globals')
  
  ## analysis tables for objective and subjective hyposmia
  ## each for the entire cohort
  ## the analysis for particular severity strata makes no point
  ## the re are too less matched observations
  
  od_kinet$analysis_tbl <- 
    list(subjective = 'anosmia_sympt', 
         objective = 'sniff_hyposmia') %>% 
    map(function(var) map(sst, select, ID, all_of(var))) %>% 
    map(reduce, inner_join, by = 'ID') %>% 
    map(set_names, c('ID', 'fup100', 'fup360'))
  
# Descriptive stats -----
  
  insert_msg('Descriptive stats')
  
  od_kinet$stats <- od_kinet$analysis_tbl %>% 
    map2(., names(.), 
         ~pivot_longer(.x, 
                       cols = c('fup100', 'fup360'), 
                       names_to = 'timepoint', 
                       values_to = .y)) %>% 
    map(mutate, timepoint = factor(timepoint, c('fup100', 'fup360'))) %>% 
    reduce(left_join, by = c('ID', 'timepoint')) %>% 
    explore(variables = c('subjective', 'objective'), 
            split_factor = 'timepoint', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', '3 months', '1 year'))
  
# Testing ------
  
  insert_msg('Testing')
  
  od_kinet$test <- od_kinet$analysis_tbl %>% 
    map(~mcnemar_test(.x$fup100, .x$fup360)) %>% 
    compress(names_to = 'variable') %>% 
    re_adjust(p_variable = 'p')
  
# Effect size -------
  
  insert_msg('Effect size')
  
  od_kinet$eff_size <- od_kinet$analysis_tbl %>% 
    map(~table(.x$fup100, .x$fup360)) %>% 
    map(cohenG) %>% 
    map(~.x[[1]]) %>% 
    compress(names_to = 'variable') %>% 
    mutate(g = abs(g)) %>% 
    as_tibble
  
  od_kinet$test <- 
    left_join(od_kinet$test, od_kinet$eff_size, by = 'variable')
  
  od_kinet$eff_size <- NULL
  od_kinet <- compact(od_kinet)
  
  ## statistics to be presented in the plot captions
  
  od_kinet$test <- od_kinet$test %>% 
    mutate(eff_size = paste('g =', signif(g, 2)), 
           plot_cap = paste('OR = ', signif(OR, 2), 
                            ', ', eff_size, 
                            ', ', significance, 
                            ', n = ', n))

# Plotting ------
  
  insert_msg('Plotting')
  
  ## representing the time changes in subjective and objective hyposmia
  ## in alluvial plots
  
  ## plotting data, frequencies expressed as percentages
  
  od_kinet$alluvial_plots$data <- od_kinet$analysis_tbl %>% 
    map(count, fup100, fup360) %>% 
    map(mutate, 
        percent = n/sum(n) * 100)
  
  ## plots
  
  od_kinet$alluvial_plots$plots <- 
    list(x = od_kinet$alluvial_plots$data, 
         y = paste0(c("Self-reported OD", 
                      "Sniffin' Stick Test OD"), 
                    ", CovILD study"), 
         z = od_kinet$test$plot_cap) %>% 
    pmap(function(x, y, z) x %>% 
           ggplot(aes(axis1 = fup100, 
                      axis2 = fup360, 
                      y = percent)) + 
           scale_x_discrete(limits = c("fup100", "fup360"), 
                            labels = c("3 months", "1 year")) + 
           geom_alluvium(aes(fill = fup100), 
                         color = 'black') + 
           geom_stratum() + 
           geom_text(stat = "stratum", 
                     aes(label = after_stat(stratum))) + 
           scale_fill_manual(values = c(no = 'steelblue', 
                                        yes = 'coral3')) + 
           guides(fill = 'none') + 
           globals$common_theme + 
           theme(axis.line = element_blank(), 
                 panel.grid.major.x = element_blank(), 
                 axis.ticks = element_blank()) + 
           labs(title = y, 
                subtitle = z, 
                x = 'Follow-up after COVID-19', 
                y = '% of complete observations'))
  
# Result table ------
  
  insert_msg('Result table')
  
  od_kinet$result_tbl <- 
    left_join(od_kinet$stats, 
              od_kinet$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable')
  
# END -------
  
  insert_tail()