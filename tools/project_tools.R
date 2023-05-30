# A medley of functional project tools

# packages ------

  library(plyr)
  library(tidyverse)
  library(rlang)
  library(trafo)
  library(ggtext)
  library(clipr)

# Sample size calculation ------

  draw_hopkins <- function(data, 
                           subset_ids, 
                           n = floor(length(subset_ids) * 0.5), 
                           seed = 1234) {
    
    ## calculates the hopkins stat for the subset of a dataset
    #ä# defined by the user-provided rownames
    
    hopkins <- get_clust_tendency(data = data[subset_ids, ], 
                                  n = n, 
                                  seed = seed)[c('hopkins_stat', 'p_value')]
    
    hopkins %>% 
      as_tibble %>% 
      mutate(n = nrow(data[subset_ids, ]))
    
  }
  
# Kinetic plot formatting -----
  
  tag_to_sub <- function(plot, replace = FALSE) {
    
    if(replace) {
      
      plot <- plot + 
        labs(subtitle = stri_replace(plot$labels$tag, 
                                     fixed = '\n', 
                                     replacement = ''))
      
    } else {
      
      plot <- plot + 
        labs(subtitle = plot$labels$subtitle %>% 
               paste(stri_replace(plot$labels$tag, 
                                  fixed = '\n', 
                                  replacement = ''), 
                     sep = ', '))
      
    }
     
    plot + 
      theme(plot.tag = element_blank())
    
  }
  

# kNN plots -----

  draw_knn_course <- function(data, 
                              time_var = 'time', 
                              dist_var = 'mean_dist', 
                              color_var = 'variable', 
                              label_var = 'variable', 
                              highlight_sympt = 'anosmia', 
                              highlight_color = 'steelblue', 
                              norm_color = 'gray60', 
                              point_size = 0.5, 
                              highlight_size = 1, 
                              line_size = 0.5, 
                              txt_size = 2.75, 
                              plot_title = NULL, 
                              plot_subtitle = NULL, 
                              plot_tag = NULL, 
                              x_lab = 'Days post CoV', 
                              y_lab = 'mean k-NN distance', 
                              cust_theme = globals$common_theme) {
    
    ## plots the course of mean KNN distance in time
    
    last_timepoint <- max(data[[time_var]])
    
    symptoms <- unique(data[[color_var]])
    
    col_scale <- rep(norm_color, length(symptoms)) %>% 
      set_names(symptoms)
    
    col_scale[highlight_sympt] <- highlight_color
    
    data <- data %>% 
      mutate(sympt_lab = ifelse(.data[[time_var]] == last_timepoint, 
                                .data[[label_var]], 
                                NA), 
             lsize_var = ifelse(.data[[color_var]] == highlight_sympt, 
                                'high', 'normal'))
    
    ## plotting
    
    ggplot(data, 
           aes(x = .data[[time_var]], 
               y = .data[[dist_var]], 
               color = .data[[color_var]])) + 
      geom_line(aes(linewidth = lsize_var)) + 
      geom_point(shape = 16, 
                 size = point_size) + 
      geom_text_repel(aes(label = sympt_lab), 
                      size = txt_size) + 
      scale_color_manual(values = col_scale, 
                         name = '') + 
      scale_linewidth_manual(values = c(normal = line_size, 
                                        high = highlight_size)) + 
      guides(color = 'none', 
             size = 'none', 
             linewidth = 'none') +
      cust_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab)
    
    
  }

# Symptom frequency plots ------
  
  draw_freq_bubble <- function(data, 
                               plot_title = NULL, 
                               plot_subtitle = NULL, 
                               plot_tag = NULL, 
                               x_lab = 'Time point', 
                               txt_size = 2.75, 
                               txt_hjust = -0.4, 
                               txt_vjust = 0.5, 
                               cust_theme = globals$common_theme) {
    
    ## bubble plot with the symptom frequencies
    
    data %>% 
      ggplot(aes(x = timepoint, 
                 y = reorder(variable, percent), 
                 fill = percent, 
                 size = percent)) + 
      geom_point(shape = 21) + 
      geom_text(aes(label = paste0(signif(percent, 2), '%')), 
                size = txt_size, 
                vjust = txt_vjust, 
                hjust = txt_hjust) +  
      scale_fill_gradient2(low = 'steelblue', 
                           mid = 'white', 
                           high = 'firebrick', 
                           midpoint = min(data$percent, na.rm = TRUE) + 
                             diff(range(data$percent, na.rm = TRUE))/2) +  
      cust_theme + 
      theme(axis.title.y = element_blank()) + 
      labs(title = plot_title,
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab)
    
  }
  
# Apriori tools -----
  
  clear_rule_tbl <- function(data, 
                             symptoms = globals$hact_symptoms, 
                             dict = hact$dict) {
    
    ## fancy look apriori result table
    
    symptoms <- sort(symptoms, decreasing = TRUE)
    
    sympt_labs <- exchange(symptoms, dict = dict)
    
    data <- data %>% 
      mutate(LHS_lab = LHS, 
             RHS_lab = RHS)
    
    for(i in symptoms) {
      
      data <- data %>% 
        mutate(LHS_lab = stri_replace_all(LHS_lab, 
                                          fixed = i, 
                                          replacement = sympt_labs[i]), 
               RHS_lab = stri_replace_all(RHS_lab, 
                                          fixed = i, 
                                          replacement = sympt_labs[i]))
      
    }
    
    data %>% 
      mutate(LHS_lab = stri_replace_all(LHS_lab, fixed = ',', replacement = ', '), 
             RHS_lab = stri_replace_all(RHS_lab, fixed = ',', replacement = ', '), 
             trans_lab = paste(LHS_lab, RHS_lab, sep = ' \u2192 '), 
             signature = paste(LHS, RHS, sep = ', '), 
             signature = stri_replace_all(signature, regex = '\\{|\\}', replacement = ''), 
             sign_lab = paste(LHS_lab, RHS_lab, sep = ', '), 
             sign_lab = stri_replace_all(sign_lab, regex = '\\{|\\}', replacement = '')) %>% 
      as_tibble
    
  }
  
  draw_conf_supp <- function(data, 
                             top_transactions = 10, 
                             by = 'confidence', 
                             label_var = 'trans_lab', 
                             embolden = NULL, 
                             cust_theme = globals$common_theme,
                             point_hjitter = 0, 
                             point_wjitter = 0, 
                             alpha_limits = c(1, 0.1), 
                             txt_color = NULL, 
                             txt_size = 2.75, 
                             plot_title = NULL, 
                             plot_subtitle = NULL, 
                             plot_tag = NULL, 
                             x_lab = 'Support', 
                             y_lab = 'Confidence', 
                             color_lab = 'Lift') {
    
    ## plots confidence versus support
    
    data <- data %>% 
      arrange(-.data[[by]]) %>% 
      mutate(top_no = 1:nrow(.)) %>% 
      mutate(top_lab = ifelse(top_no <= top_transactions, 
                              .data[[label_var]], NA))
    
    pl <- data %>% 
      ggplot(aes(x = support, 
                 y = confidence, 
                 color = lift)) + 
      geom_point(shape = 16, 
                 size = 2, 
                 position = position_jitter(width = point_wjitter, 
                                            height = point_hjitter))
    
    if(is.null(txt_color)) {
      
      pl <- pl + 
        geom_text_repel(aes(label = top_lab, 
                            fontface = ifelse(top_lab %in% embolden, 'bold', 'plain'), 
                            alpha = ifelse(top_lab %in% embolden, 'high', 'low')), 
                        size = 2.75, 
                        force = 2, 
                        force_pull = 0.5, 
                        point.padding = 0.1, 
                        min.segment.length = 0.2) + 
        scale_alpha_manual(values = c(high = alpha_limits[1], low = alpha_limits[2])) + 
        guides(alpha = 'none')
      
    } else {
      
      pl <- pl + 
        geom_text_repel(aes(label = top_lab, 
                            fontface = ifelse(top_lab %in% embolden, 'bold', 'plain'), 
                            alpha = ifelse(top_lab %in% embolden, 'high', 'low')), 
                        size = 2.75, 
                        color = txt_color, 
                        force = 2, 
                        force_pull = 0.5, 
                        point.padding = 0.1, 
                        min.segment.length = 0.2) + 
        scale_alpha_manual(values = c(high = alpha_limits[1], low = alpha_limits[2])) + 
        guides(alpha = 'none')
      
    }
    
    pl + 
      cust_theme + 
      scale_color_gradient2(low = 'steelblue', 
                            mid = 'black', 
                            high = 'firebrick', 
                            midpoint = diff(range(data$lift))/2 + min(data$lift), 
                            name = color_lab) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle,
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab)
    
  }
  
# displaying and formatting kinetic modeling results ----

  combine_plots <- function(plotlist, 
                            common_legend = c('no', 'yes', 'hide'), 
                            y_cust_range = NULL, ...) {
    
    ## combines plots sets a common scale
    
    ## common scale ranges
    
    plot_data <- plotlist %>% 
      map(~.x$data)

    if(is.null(y_cust_range)) {
      
      y_quo <- plotlist %>% 
        map(~.x$mapping$y)
      
      y_scale_range <- map2(y_quo, 
                            plot_data, 
                            eval_tidy) %>% 
        range
      
    } else {
      
      y_scale_range <- y_cust_range
      
    }
    
    ## plot panel
    
    plot_list <- plotlist %>% 
      map(function(x) x + 
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

# color setup -----

  set_colors_ <- function(color_no, seed = 123) {
    
    ## picks n colors at random from the standard palette
    
    set.seed(seed)
    
    return(colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)] %>% 
             sample(size = color_no))
    
  }
  
# varia -----
  
  vec_sum <- function(vec_list, na.rm = TRUE) {
    
    transpose(as.list(vec_list)) %>% 
      map(reduce, c) %>% 
      map_dbl(sum, na.rm = na.rm)
    
  }
  
  format_summ_tbl <- function(data, 
                              rm_n = TRUE, 
                              rm_mean = TRUE, 
                              out_value = 'axis_lab', 
                              dict = covild$dict) {
    
    ## formats a summary table with descriptive stats
    
    data <- data %>% 
      map_dfc(stri_replace, regex = 'no:.*\\nyes:\\s{1}', replacement = '') %>% 
      map_dfc(stri_replace, regex = '\\nno:.*$', replacement = '') %>% 
      map_dfc(stri_replace_all, fixed = '% (', replacement = '% (n = ') %>% 
      map_dfc(stri_replace, fixed = 'Median =', replacement = 'median:') %>% 
      map_dfc(stri_replace, fixed = 'Mean =', replacement = 'mean:') %>% 
      map_dfc(stri_replace, fixed = 'Range', replacement = 'range') %>% 
      map_dfc(stri_replace, fixed = 'Complete', replacement = 'complete') %>% 
      mutate(variable = exchange(variable, 
                                 value = out_value, 
                                 dict = dict))
    
    if(rm_n) {
      
      data <- data %>% 
        map_dfc(stri_replace, regex = '\\ncompl.*$', replacement = '')
        
    }
    
    if(rm_mean) {
      
      data <- data %>% 
        map_dfc(stri_replace, regex = 'mean.*\\n', replacement = '') %>% 
        map_dfc(stri_replace, fixed = 'median: ', replacement = '')
      
    }
    
    data
    
  }
  
  re_adjust <- function(data, p_variable = 'p_value', method = 'BH') {
    
    ## adjusts for multiple testing e.g. with the Benjamini-Hochberg method
    
    if(method != 'none') {
      
      data[['p_adjusted']] <- 
        p.adjust(data[[p_variable]], method = method)

    }
    
    data %>% 
      mutate(significance = ifelse(p_adjusted < 0.001, 
                                   'p < 0.001', 
                                   ifelse(p_adjusted >= 0.05, 
                                          paste0('ns (p = ', signif(p_adjusted, 2), ')'), 
                                          paste('p =', signif(p_adjusted, 2)))))
    
  }
  
  embolden_scale <- function(x, 
                             highlight,  
                             color = 'black', 
                             family = '', 
                             translate = FALSE, 
                             dict = hact$dict) {
    
    if(!translate) {
      
      return(ifelse(x %in% highlight, 
                    glue("<b style='color:{color}'>{x}</b>"), 
                    #paste0("<b style ='color':", color, ">", x, "</b>"), 
                    x))
      
    } else {
      
      labels <- exchange(x, dict = dict)
      
      return(ifelse(x %in% highlight, 
                    glue("<b style='color:{color}'>{labels[x]}</b>"), 
                    #paste0("<b style ='color':", color, ">", labels[x], "</b>"), 
                    labels[x]))
      
      
    }
    
  }
  
  embolden_elli <- function(plot, 
                            highlight) {
    
    ## emboldens selected elements on the Y scale
    
    plot_tbl <- plot$data[c('variable', 'plot_order')] %>% 
      mutate(variable = ifelse(variable %in% highlight, 
                               paste0('<b>', variable, '</b>'), 
                               variable))
    
    plot + 
      scale_y_continuous(limits = range(plot_tbl$plot_order), 
                         breaks = plot_tbl$plot_order, 
                         labels = set_names(plot_tbl$variable, 
                                            plot_tbl$plot_order),
                         expand = expansion(mult = c(0.02, 0.02))) + 
      theme(axis.text.y = element_markdown())
    
  }

  two_panel <- function(plot_list, 
                        legend.position = c('bottom', 'right', 'none'), 
                        rel_heights = c(0.87, 0.13)) {
    
    legend.position <- match.arg(legend.position[1], c('bottom', 'right', 'none'))
    
    plot_panel <- plot_list %>% 
      map(~.x + theme(legend.position = 'none'))
    
    if(legend.position == 'right') {
      
      plot_panel <- plot_panel %>% 
        c(list(get_legend(plot_list[[1]]))) %>% 
        plot_grid(plotlist = ., 
                  ncol = 3, 
                  align = 'hv', 
                  axis = 'tblr')
      
    } else if(legend.position == 'bottom') {
      
      plot_panel <- plot_panel %>% 
        plot_grid(plotlist = ., 
                  ncol = 2, 
                  align = 'hv', 
                  axis = 'tblr') %>% 
        plot_grid(get_legend(plot_list[[1]] + 
                               theme(legend.position = 'bottom')), 
                  nrow = 2, 
                  rel_heights = rel_heights)
      
    } else {
      
      plot_panel <- plot_panel %>% 
        plot_grid(plotlist = ., 
                  ncol = 2, 
                  align = 'hv', 
                  axis = 'tblr') %>% 
        plot_grid(ggdraw(), 
                  nrow = 2, 
                  rel_heights = rel_heights)
      
    }
    
    plot_panel
    
  }
  
# Markdown tools ------
  
  insert_issue <- function(text = NULL) {
    
    if(!is.null(text)) {
      
      text <- paste0("<span custom-style = 'reviewer'>", text, "</span>")
      
    } else {
      
      text <- paste0("<span custom-style = 'reviewer'></span>")
      
    }
    
    write_clip(content = text,
               object_type = "character",
               breaks = "\n")
    
    return(text)
    
  }
  
  insert_quotation <- function(text = NULL) {
    
    if(!is.null(text)) {
      
      text <- paste0("<span custom-style = 'reference'>", text, "</span>")
      
    } else {
      
      text <- paste0("<span custom-style = 'reference'></span>")
      
    }
    
    write_clip(content = text,
               object_type = "character",
               breaks = "\n")
    
    return(text)
    
  }
  
  insert_revision <- function(text = NULL) {
    
    if(!is.null(text)) {
      
      text <- paste0("<span custom-style = 'revision'>", text, "</span>")
      
    } else {
      
      text <- paste0("<span custom-style = 'revision'></span>")
      
    }
    
    write_clip(content = text,
               object_type = "character",
               breaks = "\n")
    
    return(text)
    
  }
  
  
# END -----