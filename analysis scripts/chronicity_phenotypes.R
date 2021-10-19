# This script investigates the phenotypes of chronic COVID-19 (day 90 on) in the survey study
# clustering in respect to the symptoms present post day 28 in at least 25% participants
# participants: only those with symptoms

  insert_head()
  
# data container -----
  
  pheno <- list()
  
# globals: clustering variables, analysis tables -----
  
  ## clustering variables
  
  pheno$variables <- rec_time$median_duration$survey %>% 
    map(filter, perc75 >= 28) %>% 
    map(~.x$symptom) %>% 
    reduce(union)
  
  ## analysis tables
  
  pheno$analysis_tbl <- kinet$analysis_tbl$survey %>% 
    map(select, 
        ID, 
        time, 
        all_of(pheno$variables)) %>% 
    map(filter, 
        time == 90) %>% 
    map(function(x) filter(x, complete.cases(x)))
  
  pheno$id_vec <- pheno$analysis_tbl %>% 
    map(~.x$ID)
  
  pheno$analysis_tbl <- pheno$analysis_tbl %>% 
    map(select, 
        - time, 
        - ID) %>% 
    map(function(x) x %>% 
          map_dfc(function(y) as.numeric(y) - 1)) %>% 
    map(data.frame) %>% 
    map2(., pheno$id_vec, set_rownames)
  
  ## filtering the individuals with chronic covid
  
  pheno$analysis_tbl <- c('survey_at', 'survey_it') %>% 
    map(function(x) pheno$id_vec[[x]] %>% 
          map_dfr(function(y) if(all(pheno$analysis_tbl[[x]][y, ] == 0)) NULL else pheno$analysis_tbl[[x]][y, ])) %>% 
    set_names(c('survey_at', 'survey_it'))
    
  
# Clustering by the SOM/HCL combi method ------
  
  insert_msg('Clustering')
  
  ## grid definition, see Vasento et al.
  
  pheno$grid_size <- pheno$analysis_tbl %>% 
    map(ncol) %>% 
    map_dbl(function(x) 5*sqrt(x)) %>% 
    min %>% 
    sqrt %>% 
    ceiling

  source('./tools/kohonen_tools.R')
  
  pheno$som_grid <- somgrid(xdim = pheno$grid_size, 
                            ydim = pheno$grid_size, 
                            topo = 'hexagonal', 
                            neighbourhood.fct = 'gaussian', 
                            toroidal = T)
  
  ## clustering

  pheno$clust_lst <- pheno$analysis_tbl %>% 
    purrr::map(clust_fcts, 
               som_grid = pheno$som_grid, 
               dist.fcts = 'tanimoto', 
               hcl_distance = 'manhattan',
               k = 3, 
               rlen = 2000)
  
  detach(package:kohonen)
  
  ## fixed assignment based on the feature density
  
  pheno$clust_lst$survey_at$assignment <- pheno$clust_lst$survey_at %>% 
    classify_clusters(clust_names = c('non-specific', 'hyposmia', 'neuro/fatigue'))
  
  pheno$clust_lst$survey_it$assignment <- pheno$clust_lst$survey_it %>% 
    classify_clusters(clust_names = c('non-specific', 'hyposmia', 'neuro/fatigue'))
  
  ## updating the analysis tables with the cluster and node assingment 
  
  pheno$analysis_tbl <- pheno$analysis_tbl %>% 
    map(rownames_to_column, 
        'ID') %>% 
    map2(., 
         map(pheno$clust_lst, ~.x$assignment), 
         left_join, 
         by = 'ID') %>% 
    map(as_tibble)
  
  ## n numbers and plot tags
  
  pheno$n_numbers <- pheno$analysis_tbl %>% 
    map(count, clust_name)
  
  pheno$plot_tags <- pheno$n_numbers %>% 
    map(function(x) map2_chr(x$clust_name, 
                             x$n, 
                             paste, 
                             sep = ': n = '))
  
# representing the symptom presence in a heat map ------
  
  insert_msg('Heat map with the cluster assignment and symptom density')
  
  pheno$heat_maps <- list(assignment_data = pheno$analysis_tbl, 
                          plot_title = paste('HACT:', globals$cohort_labels[names(pheno$analysis_tbl)])) %>% 
    pmap(plot_clust_hm, 
         symptoms = pheno$variables, 
         sympt_order = rev(c('anosmia', 'taste_loss', 
                             'imp_concentration', 'forgetfulness', 
                             'fatigue_day', 'fatigue', 
                             'breath_short')), 
         plot_subtitle = 'SOM/HCL clustering')
  
# comparing the prevalence of the clustering features -----
  
  insert_msg('Prevalence of the clustering features')
  
  ## analysis
  
  pheno$clust_ft$analysis_tbl <- pheno$analysis_tbl %>% 
    map(select, 
        - neuro_dist, 
        - clust_id) %>% 
    map(function(x) map_dfc(x, function(y) if(is.numeric(y)) factor(car::recode(y, "1 = 'yes'; 0 = 'no'")) else y))
  
  pheno$clust_ft$analyses <- pheno$clust_ft$analysis_tbl %>% 
    map(function(x) pheno$variables %>% 
          map(analyze_feature, 
              inp_tbl = x, 
              split_var = 'clust_name') %>% 
          set_names(pheno$variables))
  
  pheno$clust_ft$test_summary <- pheno$clust_ft$analyses %>% 
    map(function(x) map_dfr(x, extract_test_summary)) %>% 
    map(mutate, 
        p_adj = p.adjust(p_value, 'BH'), 
        p_label = ifelse(p_adj >= 0.05, 'ns', paste('p =', signif(p_adj, 2))))
  
  ## plots 
  
  pheno$clust_ft$plots <- pheno$clust_ft$analyses %>% 
    map2(., names(.), 
         function(x, y) list(analysis_object = x, 
                             label = paste(globals$cohort_labels[[y]], 
                                           translate_var(names(x), dict = globals$survey_var_lexicon, time_lab = F), 
                                           sep = ': ')) %>% 
           pmap(plot_analysis, 
                pie = F, 
                fill_colors = c('steelblue', 'coral3'), 
                cust_theme = globals$common_theme, 
                y_lab = '% of subset'))
  
  ## appending with the corrected p values
  
  pheno$clust_ft$plots <- map2(pheno$clust_ft$plots, 
                               map(pheno$clust_ft$test_summary, 
                                   ~.x$p_label), 
                               function(x, y) map2(x, y, 
                                                   function(plot, p_value) plot + 
                                                     labs(subtitle = p_value)))

# END ----
  
  insert_msg()