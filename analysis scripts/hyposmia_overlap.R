# This script calculates overlap between the self-reported hyposmia and other symptoms
# as a function of time

  insert_head()
  
  library(philentropy)
  
# data container -----
  
  hypo_overlap <- list()
  
# globals: analysis tables an variables ----
  
  ## reference variables
  
  hypo_overlap$ref_vars <- c('covild' = 'anosmia_sympt', 
                             'survey' = 'anosmia')
  
  ## variables 
  
  hypo_overlap$variables <- kinet$variables %>% 
    map(function(x) x[!x %in% c('sympt_present', 'anosmia_sympt', 'anosmia')])
  
  ## analysis tables, recoding the symptoms to numerics (0: absent, 1: present)

  hypo_overlap$analysis_tbl$covild <- kinet$analysis_tbl$covild %>% 
    map(function(x) x %>% 
          map_dfc(function(y) if(is.factor(y)) as.numeric(y) - 1 else y))
  
  hypo_overlap$analysis_tbl$survey <- kinet$analysis_tbl$survey %>% 
    map(function(x) x %>% 
          map_dfc(function(y) if(is.factor(y)) as.numeric(y) - 1 else y))
  
# calculation of cosine similarity coefficients, variable translation ------
  
  insert_msg('Calculation of cosine distances')

  hypo_overlap$distances <- c('covild', 'survey') %>% 
    map(function(x) hypo_overlap$analysis_tbl[[x]] %>% 
          map(get_ref_dist, 
              ref_var = hypo_overlap$ref_vars[[x]], 
              overlap_vars = hypo_overlap$variables[[x]], 
              method = 'cosine')) %>% 
    set_names(c('covild', 'survey'))
  
  ## COVILD
  
  hypo_overlap$distances <- map2(hypo_overlap$distances, 
                                 globals[c('var_lexicon', 'survey_var_lexicon')], 
                                 function(x, y) map(x, 
                                                    mutate, 
                                                    var_label = translate_var(overlap_var, 
                                                                              time_lab = F, 
                                                                              dict = y)))
  
# visualization of the distances in a time lapse ----
  
  insert_msg('Plotting the cosine similarities')

  hypo_overlap$bar_plots <- map2(c('covild', 'survey'), 
                                 globals[c('sev_labels', 'cohort_labels')], 
                                 function(x, y) list(similarity_tbl = hypo_overlap$distances[[x]], 
                                                     plot_title = y[names(hypo_overlap$distances[[x]])]) %>% 
                                   pmap(plot_similarity, 
                                        cutpoint = 0.5, 
                                        x_lab = 'Cosine coefficient', 
                                        plot_subtitle = 'Co-occurence of self-reported hyposmia and other symptoms')) %>% 
    set_names(c('covild', 'survey'))
  
# Simplified visualization as a radial plot -----
  
  insert_msg('Distance radial plots')
  
  hypo_overlap$radial_plots <- hypo_overlap$bar_plots %>% 
    map(function(x) x %>% 
          map(function(plot) plot + 
                coord_polar(theta = 'y') + 
                theme(axis.title = element_blank(), 
                      axis.text.x = element_blank(), 
                      axis.ticks.x = element_blank(), 
                      axis.line = element_blank())))
  
# Radial plots with the 5 nearest neighbors -----
  
  insert_msg('Nearest neighbor plots')
  
  hypo_overlap$knn_plots <- map2(c('covild', 'survey'), 
                                 globals[c('sev_labels', 'cohort_labels')], 
                                 function(x, y) list(similarity_tbl = hypo_overlap$distances[[x]], 
                                                     plot_title = y[names(hypo_overlap$distances[[x]])]) %>% 
                                   pmap(plot_knn,  
                                        k = 5, 
                                        cutpoint = 0.5, 
                                        fill_title = 'Cosine distance', 
                                        plot_subtitle = '5-nearest neighbors')) %>% 
    set_names(c('covild', 'survey'))
  
# END ----
  
  insert_tail()