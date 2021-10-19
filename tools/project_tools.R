# A medley of functional project tools

  require(plyr)
  require(tidyverse)
  require(furrr)
  require(caret)
  require(doParallel)
  require(randomForest)
  require(grDevices)
  require(pcaPP)
  require(rlang)
  require(cowplot)
  require(gmodels)
  require(vcd)
  
# data import and transformation -----
  
  recode_var <- function(data, old_var, new_var, ID_var = 'ID', time_var = NULL, trans_fun = 'my_identity', args = NULL) {
    
    ## renames a variable, if time and id variable names provided, they're included
    ## in  the output as well
    ## the transformation function enables
    ## args specifies the arguments to the transforming function
    
    trans_call <- call2(trans_fun, 
                        data[[old_var]], 
                        !!!args)

    new_data <- data %>% 
      mutate(!!ensym(new_var) := eval(trans_call))

    if(!is.null(time_var)) {
      
      return(new_data[c(enexpr(new_var), ID_var, time_var)])
      
    } else {
      
      return(new_data[c(enexpr(new_var), ID_var)])
      
    }
    
  }
  
  recode_vec <- function(vector, recodes, ...) {
    
    return(car::recode(vector, 
                       recodes = recodes, 
                       ...))
    
  }
  
  recode_yn <- function(vector, reverse = F, as.factor = T, ...) {
    
    ## recodes a 0/1 vector no/yes or the other way round
    
    if(reverse) {
      
      new_vec <- recode_vec(vector, 
                            "'yes' = 1;
                             'no' = 0",
                            as.factor = as.factor)
      
      #if(!as_factor) {
        
       # new_vec <- as.numeric(new_vec)
        
      #}
      
    } else {
      
      new_vec <- recode_vec(vector, 
                            "1 = 'yes';
                             0 = 'no'", 
                            as.factor = as.factor)
      
    }
    
    #if(as_factor) {
      
     # new_vec <- factor(new_vec)
      
    #}
    
    return(new_vec)
    
  }
  
  binarize <- function(vector, cutoff, labels = "'yes';'no'") {
    
    return(cut(vector, 
               c(-Inf, cutoff, Inf), 
               unlist(stri_split_fixed(labels, ';'))))
    
    
  }
  
  my_identity <- function(x, ...) {
    
    identity(x)
    
  }
  
  my_factor <- function(x, levels, ...) {
    
    factor(x, levels =  unlist(stri_split_fixed(levels, ';')))
    
  }
  
  outer_rbind <- function(tbl1, tbl2) {
    
    ## binds two data frames by rows, missing variables are filled with NA
    
    ## missing variables
    
    miss1 <- names(tbl2)[!names(tbl2) %in% names(tbl1)]
    miss2 <- names(tbl1)[!names(tbl1) %in% names(tbl2)]
    
    ## filling the tables
    
    for(i in miss1){
      
      tbl1 <- tbl1 %>% 
        mutate(!!sym(i) := NA)
      
    }
    
    for(i in miss2){
      
      tbl2 <- tbl2 %>% 
        mutate(!!sym(i) := NA)
      
    }
    
    return(rbind(tbl1, tbl2))
    
  }
  
  set_constant <- function(data, variable, reference, new_name = NULL, ID_var = 'ID', time_var = 'time') {
    
    ## takes the variable values from the reference time point and sets them accordingly 
    ## for the remaining time points
    
    ref_tbl <- data %>% ## table with the reference tables and IDs
      filter(.data[[time_var]] == reference) %>% 
      select(all_of(c(ID_var, variable)))
    
    if(!is.null(new_name)) {
      
      ref_tbl <- ref_tbl %>% 
        set_names(c(ID_var, new_name))
      
    }
    
    remain_tbl <- data %>% 
      select(- !!ensym(variable))
    
    updated_tbl <- left_join(remain_tbl, 
                             ref_tbl, 
                             by = ID_var)

    return(updated_tbl)
    
  }
  
  strat_quartile <- function(inp_tbl, numeric_variable, quant_vec = c(0.25, 0.5, 0.75), 
                             new_var_name = 'strat_var', 
                             id_index = 'ID', labels = NULL) {
    
    ## stratifies the given variable by its quartiles (or other quantiles...)
    
    if(length(numeric_variable) > 1) {
      
      out_tbl <- numeric_variable %>% 
        map(strat_quartile, 
            inp_tbl = inp_tbl, 
            new_var_name = 'strat_var', 
            id_index = id_index, 
            labels = labels) %>% 
        reduce(left_join, 
               by = id_index)
      
      if(length(numeric_variable) == length(new_var_name)) {
        
        out_tbl <- out_tbl %>% 
          set_names(c(id_index, 
                      new_var_name))
        
      }
      
      return(out_tbl)
      
    }
    
    cutoffs <- inp_tbl[[numeric_variable]] %>% 
      quantile(quant_vec, na.rm = T)
    
    cutoffs <- c(-Inf, cutoffs, Inf)
    
    out_tbl <- inp_tbl %>% 
      mutate(strat_var = cut(.data[[numeric_variable]], 
                             cutoffs, 
                             labels)) %>% 
      select(all_of(c(id_index, 
                      'strat_var'))) %>% 
      set_names(c(id_index, 
                  new_var_name))
    
    return(out_tbl)
    
  }
  
  min_max <- function(vector) {
    
    ## min max normalization of a numeric vector
    
    stopifnot(is.numeric(vector))
    
    return((vector - min(vector, na.rm = T))/(max(vector, na.rm = T) - min(vector, na.rm = T)))
    
  }
  
# displaying and formatting kinetic modeling results ----
  
  get_caption <- function(kinetic_mod_summary, 
                          term_names = c('Rec:', 'Plat:'), 
                          p_only = T) {
    
    ## extracts model betas for the recovery and chronicity term
    ## and converts them into a convenience character to show in the plot captions
    
    resp_order <- kinetic_mod_summary$response %>% 
      unique
    
    plot_cap <- kinetic_mod_summary %>% 
      filter(parameter != '(Intercept)') %>% 
      mutate(significance = ifelse(p_adj >= 0.05, 
                                   'ns', 
                                   paste('p =', signif(p_adj, 2))), 
             plot_lab = paste0('\u03B2 = ', 
                               signif(estimate, 2), 
                               ', ', 
                               significance)) %>% 
      dlply(.(response))
    
    lab_col <- if(p_only) 'significance' else 'plot_lab'
    
    plot_cap <- plot_cap %>% 
      map(function(x) map2(term_names, 
                           x[[lab_col]], 
                           paste)) %>% 
      map(paste, 
          collapse = '\n')
    
    return(plot_cap[resp_order])    
    
  }
  
  get_lrt <- function(kinetic_lrt_summary, 
                      term_names = c('Rec:', 'Plat:')) {
    
    ## extracts adjusted LRT results for the recovery and chronicity terms
    ## and converts them into a convenience character to show in the plot captions
    
    resp_order <- kinetic_lrt_summary$response %>% 
      unique
    
    plot_cap <- kinetic_lrt_summary %>% 
      filter(model != 'order_0') %>% 
      mutate(significance = ifelse(p_adj >= 0.05, 
                                   'ns', 
                                   paste('p =', signif(p_adj, 2)))) %>% 
      dlply(.(response)) %>%
      map(function(x) map2(term_names, 
                           x[['significance']], 
                           paste)) %>% 
      map(paste, 
          collapse = '\n')
    
    return(plot_cap[resp_order])    
    
    
  }
  
  combine_plots <- function(plotlist, common_legend = c('no', 'yes', 'hide'), ...) {
    
    ## combines plots sets a common scale
    
    ## common scale ranges
    
    plot_data <- plotlist %>% 
      map(~.x$data)
    
    x_quo <- plotlist %>% 
      map(~.x$mapping$x)
    
    y_quo <-  plotlist %>% 
      map(~.x$mapping$y)
    
    x_scale_range <- map2(x_quo, 
                          plot_data, 
                          eval_tidy) %>% 
      range
    
    y_scale_range <- map2(y_quo, 
                          plot_data, 
                          eval_tidy) %>% 
      range
    
    ## plot panel
    
    plot_list <- plotlist %>% 
      map(function(x) x + 
            scale_x_continuous(limits = x_scale_range) + 
            scale_y_continuous(limits = y_scale_range))
    
    common_legend <- match.arg(common_legend, 
                               choices = c('no', 'yes', 'hide'))
    
    if(common_legend == 'no') {
      
      return(plot_grid(plotlist = plot_list, ...))
      
    } else if(common_legend == 'yes') {
      
      return(plot_list %>% 
               map(function(x) x + theme(legend.position = 'none')) %>% 
               plot_grid(plotlist = ., ...) %>% 
               plot_grid(., 
                         get_legend(plot_list[[1]]), 
                         ncol = 2, 
                         rel_widths = c(0.9, 0.1)))
      
    } else {
      
      return(plot_list %>% 
               map(function(x) x + theme(legend.position = 'none')) %>% 
               plot_grid(plotlist = ., ...))
      
    }
    
  }
  
  cmm_kinet_summary <- function(ols_summary, 
                                lrt_summary, 
                                estimate_trans = identity) {
    
    ## merges the inference summary table and the LRT summary table
    ## into a common summary
    
    ## OLS
    
    ols_summary <- ols_summary %>% 
      mutate(term = car::recode(parameter, 
                                "'(Intercept)' = 'order_0'; 
                                'time' = 'order_1'; 
                                'time_sqr' = 'order_2'"), 
             estimate = estimate_trans(estimate), 
             lower_ci = estimate_trans(lower_ci), 
             upper_ci = estimate_trans(upper_ci), 
             p_ols = p_value, 
             p_adj_ols = p_adj) %>% 
      select(response, 
             time_var, 
             family, 
             ID_var, 
             term, 
             n_number, 
             estimate, 
             lower_ci, 
             upper_ci, 
             se, 
             z, 
             p_ols, 
             p_adj_ols)
    
    ## LRT
    
    lrt_summary <- lrt_summary %>% 
      mutate(term = model, 
             p_lrt = `Pr..Chisq.`, 
             p_adj_lrt = p_adj) %>% 
      select(response, 
             term, 
             AIC, 
             BIC, 
             deviance, 
             Chisq, 
             p_lrt, 
             p_adj_lrt)
    
    return(left_join(ols_summary, 
                     lrt_summary, 
                     by = c('response', 'term')))
    
  }
  
  plot_lrt_summ <- function(common_kinet_summary, 
                            label_top = 10, 
                            signif_color = 'steelblue', 
                            ns_color = 'gray80', 
                            plot_title = NULL,
                            plot_subtitle = NULL, 
                            plot_tag = NULL) {
    
    ## plotting table
    
    plotting_tbl <- common_kinet_summary %>% 
      filter(term != 'order_0') %>% 
      select(response, 
             resp_label, 
             term, 
             p_adj_lrt) %>% 
      spread(key = 'term', 
             value = 'p_adj_lrt') %>% 
      mutate(significant = ifelse(order_1 < 0.05 & order_2 < 0.05, 
                                  'yes', 'no')) %>% 
      filter(!is.na(significant))
    
    ## labeling table
    
    lab_tbl <- plotting_tbl %>% 
      filter(significant == 'yes') %>% 
      top_n(label_top, -order_2) %>% 
      mutate(plot_label = resp_label) %>% 
      select(response, 
             plot_label)
    
    plotting_tbl <- left_join(plotting_tbl, 
                              lab_tbl, 
                              by = 'response')
    
    ## plot
    
    point_plot <- plotting_tbl %>% 
      ggplot(aes(x = -log10(order_1), 
                 y = -log10(order_2), 
                 fill = significant)) + 
      geom_vline(xintercept = -log10(0.05), 
                 linetype = 'dashed') + 
      geom_hline(yintercept = -log10(0.05), 
                 linetype = 'dashed') + 
      geom_point(size = 2, 
                 shape = 21) + 
      geom_text_repel(aes(label = plot_label), 
                      size = 2.5) + 
      scale_fill_manual(values = c('yes' = signif_color, 
                                   'no' = ns_color)) + 
      guides(fill = F) + 
      globals$common_theme + 
      theme(panel.grid.major = element_line(colour = 'gray90')) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = expression('-log'[10]*'pFDR (first order)'), 
           y = expression('-log'[10]*'pFDR (second order)'))
    
    return(point_plot)
    
  }
  
  plot_beta_summ <- function(common_kinet_summary, 
                             signif_only = T, 
                             show_est_txt = F, 
                             first_color = 'steelblue3', 
                             second_color = 'coral3', 
                             plot_title = NULL,
                             plot_subtitle = NULL, 
                             plot_tag = NULL, 
                             x_trans = 'identity') {
    
    ## plots beta for the first and second order terms as a Forest plot
    
    ## plotting table
    
    plotting_tbl <- common_kinet_summary %>% 
      mutate(plot_lab = paste0(signif(estimate, 2), 
                               ' [', 
                               signif(lower_ci, 2), 
                               ' - ', 
                               signif(upper_ci, 2), 
                               ']')) %>% 
      filter(term != 'order_0', 
             !is.na(p_adj_lrt)) %>% 
      group_by(response) %>% 
      mutate(significant_lrt = if(all(p_adj_lrt < 0.05)) 'yes' else 'no',
             significant_ols = if(all(p_adj_ols < 0.05)) 'yes' else 'no')
    
    if(signif_only) {
      
      plotting_tbl <- plotting_tbl %>% 
        filter(significant_lrt == 'yes', 
               significant_ols == 'yes')
      
    }
    
    ## plot
    
    forest <- plotting_tbl %>% 
      ggplot(aes(x = estimate, 
                 y = reorder(resp_label, estimate), 
                 color = term, 
                 shape = term)) + 
      geom_vline(xintercept = 1, 
                 linetype = 'dashed') + 
      geom_errorbarh(aes(xmin = lower_ci, 
                         xmax = upper_ci), 
                     height = 0) + 
      geom_point(size = 2) + 
      scale_color_manual(values = c('order_1' = first_color, 
                                    'order_2' = second_color), 
                         labels = c('order_1' = 'first: recovery', 
                                    'order_2' = 'second: chronicity'),
                         name = 'Term order') + 
      scale_shape_manual(values = c('order_1' = 16, 
                                    'order_2' = 15), 
                         labels = c('order_1' = 'first: recovery', 
                                    'order_2' = 'second: chronicity'),
                         name = 'Term order') + 
      scale_x_continuous(trans = x_trans) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank(), 
            panel.grid.major = element_line(color = 'gray90')) + 
      facet_grid(.~term, 
                 labeller = as_labeller(c('order_1' = 'first order:\nrecovery', 
                                          'order_2' = 'second order:\nchronicity')), 
                 scales = 'free') + 
      labs(title = plot_title,
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = 'OR')
    
    if(show_est_txt) {
      
      forest <- forest + 
        geom_text(aes(label = plot_lab), 
                  size = 2.5, 
                  hjust = 0.5, 
                  vjust = -0.8)
      
    }
    
    return(forest)
    
  }

  
  
# displaying and formatting linear and LASSO modeling results -----
  
  plot_uni_forest <- function(uni_modeling_summary, 
                              plot_title = NULL, 
                              plot_subtitle = NULL, 
                              x_lab = 'OR', 
                              x_trans = 'log2', 
                              scale_labs = c('favorable', 'unfavorable', 'ns')) {
    
    ## makes a forest plot with the results of univariate modeling
    
    forest_plot <- uni_modeling_summary %>% 
      mutate(plot_lab = paste0(signif(estimate, 2), 
                               ' [', 
                               signif(lower_ci, 2), 
                               ' - ', 
                               signif(upper_ci, 2), 
                               ']'), 
             var_label = ifelse(level == 'yes', 
                                var_label, 
                                paste(var_label, level, sep = ': '))) %>% 
      ggplot(aes(x = estimate, 
                 y = reorder(var_label, estimate), 
                 color = correlation)) + 
      geom_vline(xintercept = 1, 
                 linetype = 'dashed') + 
      geom_errorbarh(aes(xmin = lower_ci, 
                         xmax = upper_ci), 
                     height = 0) + 
      geom_point(shape = 16, 
                 size = 2) + 
      geom_text(aes(label = plot_lab),
                size = 2.3, 
                vjust = -1, 
                hjust = 0.5, 
                show.legend = F) + 
      scale_x_continuous(trans = x_trans) + 
      scale_color_manual(values = c(negative = 'steelblue', 
                                    positive = 'coral3', 
                                    ns = 'gray60'), 
                         labels = scale_labs %>% 
                           set_names(c('negative', 'positive', 'ns')), 
                         name = '') +
      globals$common_theme + 
      theme(axis.title.y = element_blank(), 
            panel.grid.major = element_line(color = 'gray90')) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = if(diff(range(uni_modeling_summary$n_complete)) == 0) paste('\nn =', uni_modeling_summary$n_complete[1]) else paste('\nn =', 
                                                                                                                                    min(uni_modeling_summary$n_complete), 
                                                                                                                                    '-', 
                                                                                                                                    max(uni_modeling_summary$n_complete)), 
           x = x_lab)
    
    return(forest_plot)
    
  }
  
  plot_lasso_forest <- function(lasso_modeling_summary, 
                                plot_title = NULL, 
                                plot_subtitle = NULL, 
                                plot_tag = NULL, 
                                x_lab = 'OR', 
                                x_trans = 'identity', 
                                scale_labs = c('favorable', 'unfavorable')) {
    
    ## makes a forest plot with the results of univariate modeling
    
    forest_plot <- lasso_modeling_summary %>% 
      filter(!is.na(covariate)) %>% 
      ggplot(aes(x = estimate, 
                 y = reorder(var_lab, estimate), 
                 fill = correlation, 
                 size = abs(log(estimate)))) + 
      geom_vline(xintercept = 1, 
                 linetype = 'dashed') + 
      geom_point(shape = 21) + 
      geom_text(aes(label = signif(estimate, 2)), 
                size = 2.3, 
                vjust = -1.5) + 
      scale_x_continuous(trans = x_trans) + 
      scale_fill_manual(values = c(negative = 'steelblue', 
                                    positive = 'coral3'), 
                         labels = scale_labs %>% 
                           set_names(c('negative', 'positive')), 
                         name = '') +
      guides(size = F) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank(), 
            panel.grid.major = element_line(color = 'gray90')) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab)
    
    return(forest_plot)
    
  }
  
# variable:label translation, color setup -----
  
  translate_var <- function(variable, 
                            key = 'variable', 
                            out_value = 'label', 
                            dict = globals$survey_var_lexicon, 
                            time_lab = T, 
                            unit = F) {
    
    naming_vec <- dict[[out_value]]
    
    if(time_lab) {
      
      naming_vec <- ifelse(is.na(dict[['time_lab']]), 
                           naming_vec, 
                           paste(naming_vec, dict[['time_lab']], sep = '@'))
      
    }
    
    if(unit) {
      
      naming_vec <- ifelse(is.na(dict[['unit']]), 
                           naming_vec, 
                           paste(naming_vec, dict[['unit']], sep = ', '))
      
    }
    
    naming_vec <- set_names(naming_vec, 
                            dict[[key]])
    
    return(naming_vec[variable])
    
  }
  
  set_colors_ <- function(color_no, seed = 123) {
    
    ## picks n colors at random from the standard palette
    
    set.seed(seed)
    
    return(colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)] %>% 
             sample(size = color_no))
    
  }
  
# Kappa and factor variable overlap -----
  
  get_kappa <- function(data, variable1, variable2, kappa_only = T) {
    
    ## calculates proportions and unweighted Cohen's Kappa for two variables
    
    cross_tbl <- CrossTable(data[[variable1]], 
                            data[[variable2]], 
                            chisq = T)
    
    kappa_stat <- Kappa(cross_tbl$t)
    
    kappa_ci <- confint(kappa_stat)
    
    kappa_tbl <- tibble(kappa = kappa_stat[['Unweighted']][1], 
                        se = kappa_stat[['Unweighted']][2]) %>% 
      mutate(z = kappa/se, 
             lower_ci = kappa_ci['Unweighted', 1], 
             upper_ci = kappa_ci['Unweighted', 2], 
             p_value = 2*pnorm(z, lower.tail = F), 
             variable1 = variable1, 
             variable2 = variable2, 
             n_number = sum(cross_tbl$t)) ## two-tailed z test
    
    if(kappa_only) {
      
      return(kappa_tbl)
      
    } else {
      
      return(list(cross_tbl = cross_tbl, 
                  kappa = kappa_tbl))
      
    }
    
  }
  
  plot_kappa <- function(kappa_table, 
                         plot_title = NULL, 
                         plot_subtitle = NULL, 
                         plot_tag = NULL) {
    
    ## plots kappas between variable pairs as a heat map
    
    kappa_hm <- kappa_table %>% 
      mutate(plot_lab = paste0(signif(kappa, 2), 
                               '\n[', 
                               signif(lower_ci, 2), 
                               ' - ', 
                               signif(upper_ci, 2), 
                               ']\np = ', 
                               signif(p_adj, 2))) %>% 
      ggplot(aes(x = variable2, 
                 y = variable1,
                 fill = kappa)) + 
      geom_tile(color = 'gray40') + 
      geom_text(aes(label = plot_lab), 
                size = 2.6) + 
      scale_x_discrete(limits = globals$responses$variable, 
                       labels = translate_var(globals$responses$variable, )) + 
      scale_y_discrete(limits = globals$responses$variable, 
                       labels = translate_var(globals$responses$variable)) + 
      scale_fill_gradient2(low = 'steelblue', 
                           mid = 'white', 
                           high = 'firebrick', 
                           name = 'Kappa', 
                           midpoint = 0.5, 
                           limits = c(0, 1)) + 
      globals$common_theme + 
      theme(axis.title = element_blank(), 
            panel.grid.major = element_line(color = 'gray90')) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag)
    
    return(kappa_hm)
    
  }
  
  plot_table_object <- function(table_object, 
                                dim_names = NULL, 
                                plot_title = NULL, 
                                plot_subtitle = NULL, 
                                plot_tag = NULL, 
                                y_lab = '% of strata', 
                                fill_colors = c('steelblue', 'coral3'), 
                                txt_color = 'white') {
    
    ## plots a table object
    
    ## plotting table
    
    stopifnot(is.table(table_object))
    
    plotting_tbl <- as.data.frame(table_object)
    
    if(!is.null(dim_names)) {
      
      stopifnot(length(dim_names) == 2)
      
    } else {
      
      dim_names <- c('x', 'y')
      
    }
    
    plotting_tbl <- plotting_tbl %>% 
      set_names(c(dim_names, 'freq')) %>% 
      mutate(plot_lab = paste0(signif(freq*100, 2), '%')) %>%
      arrange(desc(.data[[dim_names[2]]])) %>% 
      group_by(.data[[dim_names[1]]]) %>% 
      mutate(lab_y = cumsum(freq) - 0.5*freq)
    
    #return(plotting_tbl)
    
    ## plot
    
    bar <- plotting_tbl %>% 
      ggplot(aes(x = .data[[dim_names[1]]], 
                 y = freq * 100, 
                 fill = .data[[dim_names[2]]])) + 
      geom_bar(stat = 'identity', 
               position = 'stack', 
               color = 'black') + 
      geom_text(aes(label = plot_lab, 
                    y = lab_y * 100), 
                size = 2.75, 
                color = txt_color) + 
      scale_fill_manual(values = fill_colors) + 
      globals$common_theme + 
      theme(panel.grid.major = element_line(color = 'gray90')) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           y = y_lab)
    
    return(bar)
    
  }
  
# Similarity calculation and result visualization ----  
  
  get_ref_dist <- function(data, 
                           ref_var = 'anosmia_sympt', 
                           time_var = 'time', 
                           ID_var = 'ID', 
                           overlap_vars = NULL, 
                           method = 'cosine', 
                           long_format = T) {
    
    ## calculates distance between the reference variable and the overlap variables 
    
    if(is.null(overlap_vars)) {
      
      overlap_vars <- names(data)[!names(data) %in% c(ref_var, time_var, ID_var)]
      
    }
    
    ## data split, removing incomplete cases
    
    data_split <- overlap_vars %>% 
      map(function(x) data[c(ID_var, time_var, ref_var, x)]) %>% 
      map(function(x) filter(x, complete.cases(x))) %>% 
      set_names(overlap_vars)
    
    ## distance/similarity calculation for particular time points
    
    dist_lst <- data_split %>% 
      map(function(x) dlply(x, time_var) %>% 
            map(~.x[3:4]) %>% 
            map(t) %>% 
            map_dbl(distance, 
                    method = method, 
                    mute.message = T))
    
    if(!method %in% c('cosine', 'ruzicka')) {
      
      ## to handle distance measures
      
      dist_lst <- dist_lst %>% 
        map(function(x) 1 - x)
      
    }

    dist_tbl <- dist_lst %>% 
      reduce(rbind) %>% 
      as_tibble %>% 
      mutate(overlap_var = names(dist_lst))
    
    if(long_format) {
      
      dist_tbl <- dist_tbl %>% 
        gather(key = !!ensym(time_var), 
               value = 'stat', 
               as.character(unique(data[[time_var]])))
      
    }
    
    return(dist_tbl)
    
  }
  
  plot_similarity <- function(similarity_tbl, 
                              y_var = 'var_label', 
                              x_var = 'stat', 
                              time_var = 'time', 
                              plot_title = NULL, 
                              plot_subtitle = NULL, 
                              plot_tag = NULL, 
                              x_lab = x_var, 
                              cust_labeller = NULL, 
                              cutpoint = NULL) {
    
    ## plots similarity between the features
    
    ## plotting table, grid formula
    
    time_var_levels <- similarity_tbl[[time_var]] %>% 
      unique

    plotting_tbl <- similarity_tbl %>% 
      mutate(!!ensym(x_var) := ifelse(is.na(.data[[x_var]]), 0, .data[[x_var]]), 
             !!ensym(time_var) := factor(.data[[time_var]], levels = time_var_levels)) ## calculation errors (symptom absent)
    
    ## grid
    
    grid_formula <- paste('.~', time_var) %>% 
      as.formula
    
    if(is.null(cust_labeller)) {
      
      cust_labeller <- paste(time_var_levels, 'days') %>% 
        set_names(time_var_levels)
      
    }
    
    ## plot
    
    bar <- plotting_tbl %>% 
      ggplot(aes(x = .data[[x_var]], 
                 y = reorder(.data[[y_var]], .data[[x_var]]), 
                 fill = .data[[x_var]])) + 
      geom_bar(stat = 'identity', 
               color = 'gray50') + 
      scale_x_continuous(limits = c(0, 1)) + 
      scale_fill_gradient2(low = 'steelblue', 
                           mid = 'white', 
                           high = 'firebrick', 
                           midpoint = 0.5, 
                           limits = c(0, 1)) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank(), 
            panel.grid.major = element_line(color = 'gray90'), 
            legend.position = 'bottom') + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           fill = x_lab) + 
      facet_grid(grid_formula, 
                 labeller = labeller(.cols = cust_labeller))
    
    if(!is.null(cutpoint)) {
      
      bar <- bar + 
        geom_vline(xintercept = 0.5, 
                   linetype = 'dashed')
      
    }

    return(bar)
    
  }
  
  plot_knn <- function(similarity_tbl, 
                       k = 10, 
                       y_var = 'var_label', 
                       x_var = 'stat', 
                       time_var = 'time', 
                       plot_title = NULL, 
                       plot_subtitle = NULL, 
                       plot_tag = NULL,
                       fill_title = NULL, 
                       cust_labeller = NULL, 
                       cutpoint = NULL, 
                       show_labels = T) {
    
    ## plots radial plots with the nearest neighbors
    
    ## plotting tbl
    
    time_var_levels <- similarity_tbl[[time_var]] %>% 
      unique
    
    plotting_tbl <- similarity_tbl %>% 
      mutate(!!ensym(x_var) := ifelse(is.na(.data[[x_var]]), 0, .data[[x_var]]), ## calculation errors (symptom absent)
             !!ensym(time_var) := factor(.data[[time_var]], levels = time_var_levels), 
             distance = 1 - .data[[x_var]]) %>% 
      group_by(.data[[time_var]]) %>% 
      top_n(n = k, 
            wt = .data[[x_var]])
    
    ## grid
    
    grid_formula <- paste('.~', time_var) %>% 
      as.formula
    
    if(is.null(cust_labeller)) {
      
      cust_labeller <- paste(time_var_levels, 'days') %>% 
        set_names(time_var_levels)
      
    }
    
    ## plot
    
    radial <- plotting_tbl %>% 
      ggplot(aes(x = distance, 
                 y = reorder(.data[[y_var]], distance), 
                 fill = distance)) +
      geom_point(shape = 21, 
                 size = 2) +
      geom_segment(aes(x = 0, 
                       xend = distance, 
                       yend = reorder(.data[[y_var]], distance))) + 
      scale_x_continuous(limits = c(0, 1)) + 
      scale_fill_gradient2(low = 'firebrick', 
                           mid = 'white', 
                           high = 'steelblue', 
                           midpoint = 0.5, 
                           limits = c(0, 1)) + 
      coord_polar(theta = 'y') + 
      globals$common_theme + 
      theme(axis.title.y = element_blank(), 
            panel.grid.major = element_line(color = 'gray90'), 
            legend.position = 'bottom', 
            axis.title = element_blank(), 
            axis.line = element_blank(), 
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           fill = fill_title) + 
      facet_grid(grid_formula, 
                 labeller = labeller(.cols = cust_labeller))
    
    if(!is.null(cutpoint)) {
      
      radial <- radial + 
        geom_vline(xintercept = 0.5, 
                   linetype = 'dashed')
      
    }
    
    if(show_labels) {
      
      radial <- radial + 
        geom_label_repel(aes(label = .data[[y_var]]), 
                         size = 2.5, 
                         label.padding = 0.1, 
                         box.padding = 0.1)
      
    }
    
    return(radial)
    
  }
  
# Symptom duration -----
  
  calculate_duration <- function(data, 
                                 symptom, 
                                 time_var = 'time', 
                                 ID_var = 'ID') {
    
    ### calculates the duration of the symptom for each individual
    
    dur_tbl <- data %>% 
      group_by(.data[[ID_var]]) %>% 
      filter(.data[[symptom]] == 'yes') %>% 
      summarise(!!ensym(time_var) := max(.data[[time_var]])) %>% 
      ungroup %>% 
      select(.data[[ID_var]], 
             .data[[time_var]]) %>% 
      set_names(c(ID_var, symptom))
    
    dur_tbl <- left_join(filter(data[, ID_var], ## to account for the individuals who have never experienced the symptom
                                !duplicated(.data[[ID_var]])), 
                         dur_tbl, 
                         by = ID_var) %>% 
      mutate(!!ensym(symptom) := ifelse(is.na(.data[[symptom]]), 0, .data[[symptom]]))
    
    return(dur_tbl)
    
  }
  
  get_duration_stats <- function(duration_tbl, 
                                 ID_var = 'ID', 
                                 cust_geom = geom_violin, 
                                 color = 'steelblue', 
                                 dict = NULL, 
                                 x_trans = 'identity', 
                                 plot_title = NULL, 
                                 plot_subtitle = NULL, 
                                 plot_tag = NULL, 
                                 median_txt = F, 
                                 x_lab = 'Time to recovery, days', 
                                 plot = T, ...) {
    
    ## plots a set of histograms with the maximal symptom duration time
    
    ## plotting table in a long format and summary table with the distribution stats
    
    symp_vars <- names(duration_tbl)[names(duration_tbl) != ID_var]
    
    plotting_tbl <- duration_tbl %>% 
      gather(key = 'symptom', 
             value = 'duration', 
             all_of(symp_vars))
    
    if(!is.null(dict)) {
      
      plotting_tbl <- plotting_tbl %>% 
        mutate(symptom = translate_var(symptom, dict = dict, time_lab = F))
      
    }
    
    summary_tbl <- plotting_tbl %>% 
      group_by(symptom) %>% 
      summarize(median = median(duration, na.rm = T), 
                perc25 = quantile(duration, 0.25, na.rm = T), 
                perc75 = quantile(duration, 0.75, na.rm = T)) %>% 
      ungroup %>% 
      arrange(median) %>% 
      mutate(plot_order = 1:nrow(.), 
             plot_label = paste0(median, 
                                 ' [', 
                                 perc25, 
                                 ' - ', 
                                 perc75, 
                                 ']'))
    
    if(!plot) {
      
      return(summary_tbl)
      
    }
    
    plotting_tbl <- left_join(plotting_tbl, 
                              summary_tbl[c('symptom', 
                                            'median', 
                                            'plot_label')], 
                              by = 'symptom')
    
    ## plot
    
    hist_plot <- plotting_tbl %>% 
      ggplot(aes(x = duration, 
                 y = reorder(symptom, median))) + 
      cust_geom(...) + 
      geom_errorbarh(data = summary_tbl, 
                     aes(x = median, 
                         xmin = perc25, 
                         xmax = perc75), 
                     height = 0, 
                     color = color) + 
      geom_point(data = summary_tbl, 
                 aes(x = median), 
                 shape = 16, 
                 size = 2, 
                 color = color) + 
      scale_x_continuous(trans = x_trans) + 
      scale_color_gradient2(low = 'steelblue', 
                            high = 'firebrick', 
                            mid = 'white', 
                            midpoint = mean(summary_tbl$median), 
                            limits = range(summary_tbl$median)) + 
      guides(fill = F) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank(), 
            panel.grid.major = element_line(color = 'gray90')) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab)
    
    if(median_txt) {
      
      hist_plot <- hist_plot + 
        geom_text(data = summary_tbl, 
                  aes(x = median, 
                      label = plot_label), 
                  size = 2.5, 
                  hjust = 0.1, 
                  vjust = -0.8, 
                  color = color)
      
    }
    
    return(hist_plot)

  }
  
# Phenotype definition by SOM ------
  
  clust_fcts <- function(data, 
                         som_grid, 
                         dist.fcts = 'tanimoto', 
                         rlen = 2000,
                         hcl_distance = 'euclidean', 
                         k = 3, 
                         hc_method = 'ward.D2', 
                         seed = 123) {
    
    ## a combi wrapper for two-step clustering of the study data:
    ## Step 1: som with the provided grid object 
    ## Step 2: hierarchical clustering with the k-branch tree cut
    
    ## SOM and SOM diagnostic plots
    
    kohonen_obj <- fit_som(data = data, 
                           grid = som_grid, 
                           dist.fcts = dist.fcts, 
                           rlen = rlen, 
                           seed = seed)
    
    kohonen_plots <- plot_som(kohonen_object = kohonen_obj)
    
    kohonen_node_ass <- get_node_ass(kohonen_object = kohonen_obj) %>% 
      set_names(c('ID', 'node', 'neuro_dist'))
    
    ## HCL
    
    clust_str <- hcluster_data(inp_tbl = kohonen_obj$codes[[1]], 
                               distance_method = hcl_distance, 
                               k = k, 
                               hc_method = hc_method, 
                               seed = seed)
    
    clust_ass <- clust_str$clust_assignment %>% 
      mutate(node = stri_replace(variable, fixed = 'V', replacement = '') %>% 
               factor) %>% 
      select(node, 
             clust_id) %>% 
      left_join(kohonen_node_ass, ., by = 'node')
    
    return(list(kohonen_obj = kohonen_obj, 
                som_plots = kohonen_plots, 
                clust_obj = clust_str, 
                assignment = clust_ass))
    
    
  }
  
  classify_clusters <- function(clust_fcts_results, clust_names = c('LR', 'IR', 'HR')) {
    
    ## assigns the cluster names based on the density of the clustering features
    
    assingment_tbl <- clust_fcts_results$assignment %>% 
      select(ID, clust_id)
    
    feature_clut_tbl <- clust_fcts_results$kohonen_obj$data[[1]] %>% 
      as.data.frame
    
    id_vec <- rownames(feature_clut_tbl)
    
    feature_sums <- id_vec %>% 
      map_dbl(function(x) sum(feature_clut_tbl[x, ], na.rm = T))
    
    feature_sum_tbl <- tibble(ID = id_vec, 
                              feature_sum = feature_sums) %>% 
      left_join(assingment_tbl, by = 'ID') %>% 
      ddply(.(clust_id), 
            summarise, 
            feature_mean = mean(feature_sum, na.rm = T)) %>% 
      arrange(feature_mean) %>% 
      mutate(clust_name = clust_names)
    
    return(left_join(clust_fcts_results$assignment, 
                     feature_sum_tbl[c('clust_id', 'clust_name')], 
                     by = 'clust_id') %>% 
             mutate(clust_name = factor(clust_name, 
                                        levels = clust_names)))
    
  }
  
  plot_clust_hm <- function(assignment_data, 
                            symptoms, 
                            sympt_order = NULL, 
                            dict = globals$survey_var_lexicon, 
                            plot_title = NULL, 
                            plot_subtitle = NULL, 
                            x_lab = 'Participant') {
    
    ## plots clustering results as a heat map
    
    n_numbers <- assignment_data %>% 
      count(clust_name)
    
    plot_tag <- map2(n_numbers$clust_name, 
                     n_numbers$n, 
                     paste, 
                     sep = ': n = ') %>% 
      paste(collapse = '\n') %>% 
      paste0('\n', .)
    
    plotting_tbl <- assignment_data %>% 
      gather(key = 'symptom', 
             value = 'present', 
             all_of(symptoms)) %>% 
      mutate(present = car::recode(present, 
                                   "1 = 'yes'; 0 = 'no'") %>% 
               as.factor)

    ## heat map
    
    heat <- plotting_tbl %>% 
      ggplot(aes(x = reorder(ID, node), 
                 y = symptom, 
                 fill = present)) + 
      facet_grid(.~ clust_name, 
                 scales = 'free', 
                 space = 'free') + 
      geom_tile() + 
      scale_fill_manual(values = c('no' = 'steelblue', 
                                   'yes' = 'coral3'), 
                        labels = c('no' = 'absent', 
                                   'yes' = 'present'), 
                        name = '') + 
      scale_y_discrete(labels = translate_var(symptoms, dict = dict, time_lab = F), 
                       limits = sympt_order) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank(), 
            axis.text.x = element_blank(), 
            axis.line.x = element_blank(), 
            axis.ticks.x = element_blank()) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab)
    
    return(heat)
    
  }
  
# varia -----
  
  vec_sum <- function(vec_list, na.rm = T) {
    
    transpose(as.list(vec_list)) %>% 
      map(reduce, c) %>% 
      map_dbl(sum, 
              na.rm = na.rm)
    
  }

  mm_inch <- function(input_mm) {
    
    return(0.0393700787 * input_mm)
    
  }
  
  set_rownames <- function(inp_tbl, new_rownames) {
    
    ## sets custom rownames
    
    out_tbl <- inp_tbl %>% 
      as.data.frame
    
    rownames(out_tbl) <- new_rownames
    
    return(out_tbl)      
    
  }
  
  get_tag <- function(plot) {
    
    ## extracts the plot tag and changes it's format for the figure panel
    
    plot_tag <- plot %>% 
      ggplot_build
    
    plot_tag <- plot_tag$plot$labels$tag
    
    return(plot_tag)
    
  }
  
# report -----
  
  paste_range <- function(numeric_vector) {
    
    stopifnot(is.numeric(numeric_vector))
    
    vec_range <- range(numeric_vector, na.rm = T)
    
    if(diff(vec_range) == 0) {
      
      return(paste('n =', vec_range[1]))
      
    } else {
      
      return(paste('n =', vec_range[1], '-', vec_range[2]))
      
    }
    
  }
  
# END -----