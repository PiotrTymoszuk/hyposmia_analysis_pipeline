# Kinetic of the Sniffin Stick test results in the CovILD cohort
# No analyses for severity strata done, due to low power

  insert_head()
  
# container ------
  
  sst_kinet <- list()
  
# analysis globals -----
  
  insert_msg('Analysis globals')
  
  sst_kinet$complete_ids <- sst %>% 
    map(~.x$ID) %>% 
    reduce(intersect)
  
  sst_kinet$analysis_tbl <- sst %>% 
    map(filter, ID %in% sst_kinet$complete_ids) %>% 
    map(arrange, ID) %>% 
    map(select, ID, sniff_score) %>% 
    set_names(c('3 months', '1 year')) %>% 
    compress(names_to = 'timepoint') %>% 
    mutate(timepoint = factor(timepoint, c('3 months', '1 year')))
  
# Descriptive stats ------
  
  insert_msg('Descriptive stats')
  
  sst_kinet$stats <- sst_kinet$analysis_tbl %>% 
    explore(split_factor = 'timepoint', 
            variables = 'sniff_score', 
            what = 'table', 
            pub_styled = TRUE) %>% 
    reduce(left_join, by = 'variable') %>% 
    set_names(c('variable', levels(sst_kinet$analysis_tbl$timepoint)))
  
# Testing for differences of SST at the follow-ups ------
  
  insert_msg('Testing')
  
  sst_kinet$test <- sst_kinet$analysis_tbl %>% 
    compare_variables(variables = 'sniff_score', 
                      split_factor = 'timepoint', 
                      what = 'eff_size', 
                      types = 'paired_wilcoxon_r', 
                      exact = FALSE, 
                      ci = FALSE, 
                      pub_styled = TRUE) %>% 
    mutate(plot_cap = paste(eff_size, significance, sep = ', '))
  
# Plotting ------
  
  insert_msg('Plotting')
  
  ## before-after
  
  sst_kinet$plots <- 
    list(type = c(paired = 'paired', 
                  box = 'box')) %>% 
    pmap(plot_variable, 
         sst_kinet$analysis_tbl, 
         variable = 'sniff_score', 
         split_factor = 'timepoint', 
         point_hjitter = 0.1, 
         cust_theme = globals$common_theme, 
         x_n_labs = TRUE, 
         plot_title = "Sniffin' Stick Test, CovILD study", 
         plot_subtitle = sst_kinet$test$plot_cap, 
         x_lab = 'Follow-up after COVID-19', 
         y_lab = 'points') %>% 
    map(~.x + 
          scale_fill_manual(values = c('steelblue', 'steelblue')) + 
          guides(fill = 'none'))
  
# Result table ------
  
  insert_msg('Result table')

  sst_kinet$result_tbl <- 
    left_join(sst_kinet$stats, 
              sst_kinet$test[c('variable', 'significance', 'eff_size')], 
              by = 'variable')
  
# END ------
  
  insert_tail()