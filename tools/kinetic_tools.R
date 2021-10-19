# This scrip provided tool for first and second term linear modling of the variable's kinetics

# toolbox -----

  require(tidyverse)
  require(rlang)
  require(lme4)
  require(lmerTest)
  require(stringi)
  require(ggrepel)
  require(furrr)

# helper functions -----

  identity_matrix_ <- function(dimension) {
    
    ## creates an identity matrix with a given dimension
    
    out_matrix <- matrix(0, dimension, dimension)
    
    for (i in 1:dimension) {
      
      out_matrix[i, i] <- 1
      
    }
    
    return(out_matrix)
    
  }

  get_lme_results_ <- function(model_mix, L_matrix = NULL, anova = F, ...){
    
    require(lmerTest)
    
    ## a function returning confidence intervals and significances for particular coefficents of the mixed model with one fixed
    ## effect. The coefficients to display are defined by the L_matrix. Uses the contest function from the lmerTest package
    ## If the L_matrix is not given, stats for all coefficients are calculated

    if(is.null(L_matrix)) {
      
      L_matrix <- identity_matrix_(length(model_mix@beta))
      
    }
    
    if(anova) {
      
      output <- contest(model_mix, L_matrix, ...)
      
    } else {
      
      output <- contest(model_mix, L_matrix, joint = F, ...) %>% 
        as_tibble

      coef_names <- rownames(summary(model_mix)$coefficients) %>% 
        as_tibble
      
      output <- cbind(coef_names, output) %>%
        set_names(c('parameter', 
                    'estimate', 
                    'se', 
                    'df', 
                    't', 
                    'lower_ci', 
                    'upper_ci', 
                    'p_value'))
      
    }
    
    return(output)
  }
  
  get_glm_results_ <- function(glm_model, exponentiate = F, default_signif = 2) {
    
    # retrieves regression estimate stats from a glm
    
    ## model coefficients
    
    coefs <- glm_model %>% 
      summary %>% 
      coefficients %>% 
      data.frame()
    
    estimates <- coefs[, c('Estimate', 'Std..Error')] %>% 
      as_tibble %>% 
      set_names(c('estimate', 'se'))
    
    parameters <- data.frame(parameter = rownames(coefs))
    
    stats <- coefs[, c('z.value', 'Pr...z..')] %>% 
      as_tibble %>% 
      set_names(c('z', 'p_value'))
    
    n_number <- nrow(model.frame(glm_model))
    
    ## inference
    
    ci <- try(glm_model %>% 
                confint %>% 
                as_tibble %>% 
                set_names(c('lower_ci', 'upper_ci')), 
              silent = T)
    
    if(any(class(ci) == 'try-error')) {

      ci <- tibble(lower_ci = coefs[, 'Estimate'] + coefs[, 'Std..Error'] * qnorm(0.025), 
                   upper_ci = coefs[, 'Estimate'] + coefs[, 'Std..Error'] * qnorm(0.975))
      
      warning('Falling back to CI calculation from normal distribution', 
              call. = F)
      
    } else {
      
      if(any(class(glm_model) == 'glmerMod')) {
        
        ci <- ci[-1, ] ## dropping out the CI estimates for sigma
        
      }
      
    }

    ## output
    
    if(!exponentiate) {
      
      summary_table <- cbind(parameters, estimates, ci, stats)
      
    } else {
      
      summary_table <- cbind(parameters, exp(estimates), exp(ci), stats)
      
    }
    
    summary_table <- summary_table %>% 
      mutate(n_number = n_number, 
             plot_label = paste(signif(estimate, default_signif), 
                                ' [', 
                                signif(lower_ci, default_signif), 
                                ' - ', 
                                signif(upper_ci, default_signif), 
                                '], p = ', 
                                signif(p_value, default_signif), sep = ''))
    
    return(summary_table)
    
    
  }
  
  plot_kin_numeric_ <- function(kinetic_object, 
                                point_color = 'gray60', 
                                resp_color = 'steelblue', 
                                fitted_color = 'coral3', 
                                fitted_line = 'dashed', 
                                cust_theme = theme_classic(), 
                                jitter_w_perc = 2, 
                                jitter_h_perc = 1, 
                                plot_title = NULL, 
                                plot_subtitle = NULL, 
                                plot_tag = NULL, 
                                x_lab = kinetic_object$time_var, 
                                y_lab = kinetic_object$response, 
                                iqr_resp = T, 
                                hide_fitted = F, 
                                iqr_fitted = F) {
    
    ## plots the given kinetic object
    
    ## plotting tables
    
    plot_tbl <- predict(kinetic_object)
    
    time_var <- kinetic_object$time_var
    
    real_resp <- kinetic_object$response
    
    fitted_resp <- paste0(kinetic_object$response, '_fitted')
    
    ## handling the categorized responses which may be present in binomial modeling
    
    if(is.factor(plot_tbl[[real_resp]])) {
      
      plot_tbl <- plot_tbl %>% 
        mutate(!!sym(real_resp) := as.numeric(.data[[real_resp]]) - 1)
      
    }
    
    ## setting up the point jitters and the tag
    
    jitter_w = jitter_w_perc * diff(range(plot_tbl[[time_var]], na.rm = T))/100
    
    jitter_h = jitter_h_perc * diff(range(plot_tbl[[real_resp]], na.rm = T))/100
    
    if(is.null(plot_tag)) {
      
      n_numbers <- plot_tbl %>% 
        count(.data[[time_var]]) %>% 
        .$n %>% 
        range
      
      if(diff(n_numbers) != 0) {
        
        plot_tag <- paste0('\nn = ', 
                           n_numbers[1], 
                           ' - ', 
                           n_numbers[2])
        
      } else {
        
        plot_tag <- paste('\nn =', 
                           n_numbers[1])
        
      }
      
    }
    
    ## summary tables with the outcome and fitted medians/IQR
    
    summary_real <- plot_tbl %>% 
      group_by(!!sym(kinetic_object$time_var)) %>% 
      summarise(median = median(.data[[real_resp]]), 
                perc25 = quantile(.data[[real_resp]], 0.25), 
                perc75 = quantile(.data[[real_resp]], 0.75), 
                type = 'real')
    
    if(hide_fitted) {
      
      summary_tbl <- summary_real
      
    } else {
      
      summary_fitted <- plot_tbl %>% 
        group_by(!!sym(kinetic_object$time_var)) %>% 
        summarise(median = median(.data[[fitted_resp]]), 
                  perc25 = quantile(.data[[fitted_resp]], 0.25), 
                  perc75 = quantile(.data[[fitted_resp]], 0.75), 
                  type = 'fitted')
      
      summary_tbl <- rbind(summary_real, 
                           summary_fitted)
      
    }
    
    ## kinetic plot
    
    kinet_plot <- plot_tbl %>% 
      ggplot(aes(x = .data[[time_var]], 
                 y = .data[[real_resp]])) + 
      geom_line(data = summary_tbl, 
                aes(y = median, 
                    color = type, 
                    linetype = type))
    
    if(iqr_resp) {
      
      kinet_plot <- kinet_plot + 
        geom_ribbon(data = summary_tbl %>% 
                      filter(type == 'real'), 
                    aes(y = median, 
                        ymin = perc25, 
                        ymax = perc75, 
                        fill = type), 
                    alpha = 0.1, 
                    color = 'gray80')
      
    }
    
    if(all(iqr_fitted, !hide_fitted)) {
      
      kinet_plot <- kinet_plot + 
        geom_ribbon(data = summary_tbl %>% 
                      filter(type == 'fitted'), 
                    aes(y = median, 
                        ymin = perc25, 
                        ymax = perc75, 
                        fill = type), 
                    alpha = 0.1, 
                    color = 'gray80')
      
    }
    
    kinet_plot <- kinet_plot + 
      geom_point(shape = 21, 
                 size = 2, 
                 alpha = 0.5, 
                 fill = point_color, 
                 position = position_jitter(width = jitter_w, 
                                            height = jitter_h)) + 
      scale_color_manual(values = c(real = resp_color, 
                                    fitted = fitted_color), 
                         labels = c(real = 'Actual', 
                                    fitted = 'Fitted'), 
                         name = '') + 
      scale_fill_manual(values = c(real = resp_color, 
                                   fitted = fitted_color), 
                        labels = c(real = 'Actual', 
                                   fitted = 'Fitted'), 
                        name = '') + 
      scale_linetype_manual(values = c(real = 'solid', 
                                       fitted = fitted_line), 
                            labels = c(real = 'Actual', 
                                       fitted = 'Fitted'), 
                            name = '') + 
      guides(fill = F) + 
    cust_theme + 
      theme(panel.grid.major = element_line(color = 'gray90'), 
            plot.tag.position = 'bottom') + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab)
    
    return(kinet_plot)
    
    
  }
  
  plot_kin_fct_ <- function(kinetic_object, 
                            point_color = 'steelblue', 
                            resp_color = 'steelblue', 
                            fitted_color = 'coral3', 
                            fitted_line = 'dashed', 
                            cust_theme = theme_classic(), 
                            plot_title = NULL, 
                            plot_subtitle = NULL, 
                            plot_tag = NULL, 
                            x_lab = kinetic_object$time_var, 
                            y_lab = '% cohort', 
                            show_labels = T, 
                            repel_labels = F, 
                            signif_digits = 2, 
                            label_size = 2.3, 
                            hide_fitted = F) {
    
    ## plots the given kinetic object
    
    ## plotting tables
    
    plot_tbl <- predict(kinetic_object)
    
    time_var <- kinetic_object$time_var
    
    real_resp <- kinetic_object$response
    
    fitted_resp <- paste0(kinetic_object$response, '_fitted')
    
    ## handling the categorized responses which may be present in binomial modeling
    
    if(is.factor(plot_tbl[[real_resp]])) {
      
      plot_tbl <- plot_tbl %>% 
        mutate(!!sym(real_resp) := as.numeric(.data[[real_resp]]) - 1)
      
    }
    
    ## setting up the plot tag
    
    if(is.null(plot_tag)) {
      
      n_numbers <- plot_tbl %>% 
        count(.data[[time_var]]) %>% 
        .$n %>% 
        range
      
      if(diff(n_numbers) != 0) {
        
        plot_tag <- paste0('\nn = ', 
                           n_numbers[1], 
                           ' - ', 
                           n_numbers[2])
        
      } else {
        
        plot_tag <- paste('\nn =', 
                          n_numbers[1])
        
      }
      
    }    
    ## summary tables with the outcome prevalence in the subsequent time points
    
    summary_real <- plot_tbl %>% 
      group_by(!!sym(kinetic_object$time_var)) %>% 
      summarise(prevalence = mean(.data[[real_resp]]),
                type = 'real')
    
    if(hide_fitted) {
      
      summary_tbl <- summary_real
      
    } else {
      
      summary_fitted <- plot_tbl %>% 
        group_by(!!sym(kinetic_object$time_var)) %>% 
        summarise(prevalence = mean(.data[[fitted_resp]]), 
                  type = 'fitted')
      
      summary_tbl <- rbind(summary_real, 
                           summary_fitted)
      
    }
    
    summary_tbl <- summary_tbl %>% 
      mutate(point_lab = ifelse(type == 'real', 
                                signif(prevalence*100, signif_digits), 
                                NA))
    
    ## kinetic plot
    
    kinet_plot <- summary_tbl %>% 
      ggplot(aes(x = .data[[time_var]], 
                 y = prevalence * 100)) + 
      geom_line(aes(color = type, 
                    linetype = type)) + 
      geom_point(data = summary_tbl %>% 
                   filter(type == 'real'), 
                 shape = 21, 
                 size = 2, 
                 fill = point_color) + 
      scale_color_manual(values = c(real = resp_color, 
                                    fitted = fitted_color), 
                         labels = c(real = 'Actual', 
                                    fitted = 'Fitted'), 
                         name = '') + 
      scale_fill_manual(values = c(real = resp_color, 
                                   fitted = fitted_color), 
                        labels = c(real = 'Actual', 
                                   fitted = 'Fitted'), 
                        name = '') + 
      scale_linetype_manual(values = c(real = 'solid', 
                                       fitted = fitted_line), 
                            labels = c(real = 'Actual', 
                                       fitted = 'Fitted'), 
                            name = '') + 
      cust_theme + 
      theme(panel.grid.major = element_line(color = 'gray90'), 
            plot.tag.position = 'bottom') + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           x = x_lab, 
           y = y_lab)
    
    if(show_labels) {
      
      if(!repel_labels) {
        
        kinet_plot <- kinet_plot + 
          geom_text(aes(label = point_lab), 
                    size = label_size, 
                    hjust = 0.25, 
                    vjust = -1,
                    color = point_color)
        
      } else {
        
        kinet_plot <- kinet_plot + 
          geom_text_repel(aes(label = point_lab), 
                          size = label_size, 
                          hjust = 0.25, 
                          vjust = -1,
                          color = point_color, 
                          box.padding = 0.1, 
                          direction = 'x')
        
      }
      
    }
    
    return(kinet_plot)
    
  }

# modeling functions, generating a kinetic object ----

  model_kinetic <- function(data, 
                            response, 
                            time_var = 'time_numeric', 
                            ID_var = 'ID', 
                            family = 'gaussian', 
                            order = c(2, 1, 0), 
                            time_subset = NULL, ...) {
    
    ## models the numeric variable in time
    
    if(!any(class(data) == 'data.frame')) {
      
      stop('A data frame needed as data argument')
      
    }
    
    if(!all(response %in% names(data) & time_var %in% names(data) & ID_var %in% names(data))) {
      
      stop('A variable absent from the input data frame')
      
    }
    
    if(!is.null(time_subset)) {
      
      data <- data %>% 
        filter(.data[[time_var]] %in% time_subset)
      
    }
    
    mod_frame <- data %>% 
      select(all_of(c(ID_var, 
                      response, 
                      time_var))) %>% 
      mutate(!!sym(paste0(time_var, '_sqr')) := .data[[time_var]]^2) %>% 
      filter(complete.cases(.))
    
    order <- order[1] %>% 
      as.character
    
    mod_formula <- switch(order, 
                          '0' = expr(!!ensym(response) ~ (1|!!ensym(ID_var))), 
                          '1' = expr(!!ensym(response) ~ !!ensym(time_var) + (1|!!ensym(ID_var))), 
                          '2' = expr(!!ensym(response) ~ !!ensym(time_var) + !!sym(paste0(time_var, '_sqr')) + (1|!!ensym(ID_var)))) %>% 
      as.formula(env = caller_env())
    
    if(family == 'gaussian') {
      
      model <- lmer(formula = mod_formula, 
                    data = mod_frame, ...)
      
    } else {
      
      model <- glmer(formula = mod_formula, 
                     data = mod_frame,
                     family = family, ...)
      
    }
    
    ## output object
    
    kinet_obj <- structure(.Data = list(model = model, 
                                        response = response, 
                                        ID_var = ID_var, 
                                        time_var = time_var, 
                                        family = family, 
                                        order = order, 
                                        time_subset = time_subset), 
                           class = 'kinetic')
    
    return(kinet_obj)    
    
  }
  
  model_kinetic_lst <- function(data, 
                                responses = NULL, 
                                time_var = 'time_numeric', 
                                ID_var = 'ID', 
                                family = 'gaussian', 
                                order = 2, 
                                time_subset = NULL, 
                                .parallel = F, 
                                .errors = F, ...) {
    
    ## a wrapper handling the lists/vectors of responses
    
    start_time <- Sys.time()
    message(paste('Creating', length(responses), 'kinetic models'))
    on.exit(message(paste('Elapsed', Sys.time() - start_time)))
    
    if(.parallel) {
      
      plan('multisession')
      
      kinet_lst <- responses %>% 
        future_map(safely(model_kinetic), 
                   data = data, 
                   time_var = time_var, 
                   ID_var = ID_var, 
                   family = family, 
                   order = order, 
                   time_subset = time_subset, ..., 
                   .options = furrr_options(packages = c('tidyverse', 
                                                         'rlang', 
                                                         'lme4', 
                                                         'lmerTest'), 
                                            globals = F)) %>% 
        set_names(responses)
      
      plan('sequential')
      
    } else {
      
      kinet_lst <- responses %>% 
        map(safely(model_kinetic), 
            data = data, 
            time_var = time_var, 
            ID_var = ID_var, 
            family = family, 
            order = order, 
            time_subset = time_subset, ...) %>% 
        set_names(responses)
      
    }
    
    kinet_lst <- transpose(kinet_lst) %>% 
      map(compact)
    
    if(.errors) {
      
      return(kinet_lst)
      
    } else {
      
      return(kinet_lst$result)
      
    }
    
  }
  
  inference_kinetic_list <- function(kinetic_object_lst, 
                                     adj_method = 'BH', 
                                     .parallel = F, 
                                     .errors = F, ...) {
    
    ## a wrapper obtaining fixed-effect beta inference for a list of kinetic objects
    
    if(!is.list(kinetic_object_lst)) {
      
      stop('The function requires a list of kinetic objects')
      
    }
    
    class_check <- map_chr(kinetic_object_lst, class)
    
    if(!all(class_check == 'kinetic')) {
      
      stop('Non-kinetic class object provided')
      
    }
    
    start_time <- Sys.time()
    message(paste('Inference for', length(kinetic_object_lst), 'kinetic model objects'))
    on.exit(message(paste('Elapsed', Sys.time() - start_time)))
    
    if(.parallel) {
      
      plan('multisession')
      
      coef_lst<- kinetic_object_lst %>% 
        future_map(safely(coef), ..., 
                   .options = furrr_options(packages = c('tidyverse', 
                                                         'rlang', 
                                                         'lme4', 
                                                         'lmerTest'), 
                                            globals = c('coef.kinetic', 
                                                        'model.frame.kinetic', 
                                                        'get_glm_results_', 
                                                        'get_lme_results_', 
                                                        'identity_matrix_')))
      
      plan('sequential')
      
    } else {
      
      coef_lst<- kinetic_object_lst %>% 
        map(safely(coef), ...)
      
    }
    
    coef_lst <- transpose(coef_lst) %>% 
      map(compact)
    
    coef_lst$result <- tryCatch(coef_lst$result %>% 
                                  reduce(rbind) %>% 
                                  mutate(p_adj = p.adjust(p_value, adj_method)), 
                                error = function(e) coef_lst$error)
    
    if(.errors) {
      
      return(coef_lst)
      
    } else {
      
      return(coef_lst$result)
      
    }
    
  }

# methods for the kinetics object -----

  coef.kinetic <- function(kinetic_object) {
    
    ## returns kinetic model coefficients
    
    if(kinetic_object$family == 'gaussian') {
      
      coefs <- get_lme_results_(model_mix = kinetic_object$model, 
                                L_matrix = NULL, 
                                anova = F) %>% 
        as_tibble %>% 
        mutate(n_number = nrow(model.frame(kinetic_object)), 
               response = kinetic_object$response, 
               time_var = kinetic_object$time_var, 
               family = kinetic_object$family, 
               ID_var = kinetic_object$ID_var)
      
    } else {
      
      ## for other families
      
      coefs <- get_glm_results_(glm_model = kinetic_object$model, 
                                exponentiate = F) %>% 
        mutate(response = kinetic_object$response, 
               time_var = kinetic_object$time_var, 
               family = kinetic_object$family, 
               ID_var = kinetic_object$ID_var) %>% 
        select(parameter, 
               estimate, 
               se, 
               z, 
               lower_ci, 
               upper_ci, 
               p_value, 
               n_number, 
               response, 
               time_var, 
               family, 
               ID_var) %>% 
        as_tibble()
      
      if(any(is.na(c(coefs$lower_ci, coefs$upper_ci)))) {
        
        coefs <- coefs %>% 
          mutate(lower_ci = estimate + se * qnorm(0.025), 
                 upper_ci = estimate + se * qnorm(0.975))
        
        warning('Falling back to CI calculation from normal distribution', 
                call. = F)
        
      }
      
    }
    
    return(coefs)
    
  }

  anova.kinetic <- function(kinetic_object) {
    
    ## returns kinetic model coefficients
    
    if(kinetic_object$family == 'gaussian') {
      
      anova_res <- get_lme_results_(model_mix = kinetic_object$model, 
                                    L_matrix = NULL, 
                                    anova = T) %>% 
        as_tibble %>% 
        mutate(response = kinetic_object$response, 
               time_var = kinetic_object$time_var, 
               family = kinetic_object$family, 
               ID_var = kinetic_object$ID_var)
      
      
    } else {
      
      anova_res <- anova(kinetic_object$model)
      
    }
    
    return(anova_res)
    
  }

  summary.kinetic <- function(kinetic_object) {
    
    list(summary = summary(kinetic_object$model), 
         coefs = coef(kinetic_object))
    
}

  print.kinetic <- function(kinetic_object) {
    
    print(summary(kinetic_object$model))
    
}

  predict.kinetic <- function(kinetic_object, type = 'response') {
    
    mod_frame <- model.frame(kinetic_object) %>% 
      mutate(!!sym(paste0(kinetic_object$response, '_fitted')) := predict(kinetic_object$model, 
                                                                          type = type)) %>% 
      as_tibble
    
    return(mod_frame)
    
  }
  
  formula.kinetic <- function(kinetic_object) {
    
    formula(kinetic_object$model)
    
  }
  
  model.frame.kinetic <- function(kinetic_object) {
    
    model.frame(kinetic_object$model)
    
  }
  
  plot.kinetic <- function(kinetic_object, 
                           point_color = 'steelblue', 
                           resp_color = 'steelblue', 
                           fitted_color = 'coral3', 
                           fitted_line = 'dashed', 
                           cust_theme = theme_classic(), 
                           plot_title = NULL, 
                           plot_subtitle = NULL, 
                           plot_tag = NULL, 
                           x_lab = kinetic_object$time_var,
                           type = c('kinetics', 'prevalence'), ...) {
    
    ## plots the given kinetic object
    ## for binomial modeling results both the feature prevalence 
    
    
    
    ## plotting tables
    
    if(kinetic_object$family != 'binomial' || type == 'kinetics') {
      
      return(plot_kin_numeric_(kinetic_object = kinetic_object, 
                               point_color = point_color, 
                               resp_color = resp_color, 
                               fitted_color = fitted_color, 
                               fitted_line = fitted_line, 
                               cust_theme = cust_theme, 
                               plot_title = plot_title, 
                               plot_subtitle = plot_subtitle, 
                               plot_tag = plot_tag,  
                               x_lab = x_lab, ...))
      
    } else {
      
      return(plot_kin_fct_(kinetic_object = kinetic_object, 
                           point_color = point_color, 
                           resp_color = resp_color, 
                           fitted_color = fitted_color, 
                           fitted_line = fitted_line, 
                           cust_theme = cust_theme, 
                           plot_title = plot_title, 
                           plot_subtitle = plot_subtitle, 
                           plot_tag = plot_tag, 
                           x_lab = x_lab, ...))
      
    }

  }
  
  AIC.kinetic <- function(kinetic_object) {
    
    AIC(kinetic_object$model)
    
  }
  
  BIC.kinetic <- function(kinetic_object) {
    
    BIC(kinetic_object$model)
    
  }
  
  deviance.kinetic <- function(kinetic_object, ...) {
    
    deviance(kinetic_object$model, ...)
    
  }
  
  residuals.kinetic <- function(kinetic_object) {
    
    residuals(kinetic_object$model)
    
  }
  
  length.kinetic <- function(kinetic_object) {
    
    ## extracts the number of complete observations from a model
    
    length(unique(model.frame(kinetic_object)[[kinetic_object$ID_var]]))
    
  }
  
# Quality control -----
  
  lrt <- function(kinetic_object, verbose = F, ...) {
    
    ## performs a step-wise LRT for the time term
    
    if(class(kinetic_object) != 'kinetic') {
      
      stop('The function requires a kinetic object')
      
    }
    
    if(verbose) {
      
      message('LRT modeling')
      start_time <- Sys.time()
      
      on.exit(message(paste('Elapsed:', Sys.time() - start_time)))
      
    }
    
    nested_models <- kinetic_object$order:0 %>% 
      map(model_kinetic, 
          data = model.frame(kinetic_object), 
          response = kinetic_object$response, 
          time_var = kinetic_object$time_var, 
          ID_var = kinetic_object$ID_var, 
          family = kinetic_object$family, 
          time_subset = kinetic_object$time_subset) %>% 
      set_names(paste0('order_', 
                       kinetic_object$order:0))

    ## calculating AIC, BIC and deviance
    
    stat_tbl <- tibble(model = names(nested_models), 
                       aic = map_dbl(nested_models, AIC), 
                       bic = map_dbl(nested_models, BIC), 
                       deviance = map_dbl(nested_models, deviance, REML = F))
    
    ## LRT

    lrt_res <- 1:length(nested_models) %>% 
      map(function(x) try(anova(nested_models[[x + 1]]$model, 
                                nested_models[[x]]$model, ...), 
                          silent = T)) %>% 
      map(function(x) if(class(x) == 'try-error') NULL else x) %>% 
      compact

    lrt_tbl <- lrt_res %>% 
      map(~.x[2, ]) %>% 
      map_dfr(data.frame) %>% 
      rbind(data.frame(lrt_res[[length(lrt_res)]][1, ])) %>% 
      mutate(model = names(nested_models), 
             response = kinetic_object$response) %>% 
      as_tibble
 
    return(lrt_tbl)
    
  }
  
  lrt_list <- function(kinetic_object_lst, adj_method = 'BH', .parallel = F, .errors = F, ...) {
    
    ## a list wrapper enabling parallel computation
    
    if(!is.list(kinetic_object_lst)) {
      
      stop('The function requires a list of kinetic objects')
      
    }
    
    class_check <- map_chr(kinetic_object_lst, class)
    
    if(!all(class_check == 'kinetic')) {
      
      stop('Non-kinetic class object provided')
      
    }
    
    start_time <- Sys.time()
    message(paste('LRT check for', length(kinetic_object_lst), 'kinetic model objects'))
    on.exit(message(paste('Elapsed', Sys.time() - start_time)))
    
    if(.parallel) {
      
      plan('multisession')
      
      lrt_result_lst <- lrt_result_lst <- kinetic_object_lst %>% 
        future_map(safely(lrt), 
                   verbose = F, ..., 
                   .options = furrr_options(packages = c('tidyverse', 
                                                         'rlang', 
                                                         'lme4', 
                                                         'lmerTest', 
                                                         'stringi'), 
                                            globals = c('model_kinetic', 
                                                        'model.frame.kinetic', 
                                                        'AIC.kinetic', 
                                                        'BIC.kinetic', 
                                                        'deviance.kinetic')))
      
      plan('sequential')
      
    } else {
      
      lrt_result_lst <- kinetic_object_lst %>% 
        map(safely(lrt), 
            verbose = F, ...)
      
    }
    
    lrt_result_lst <- transpose(lrt_result_lst) %>% 
      map(compact)
    
    lrt_result_lst$result <- tryCatch(lrt_result_lst$result %>% 
                                        reduce(rbind) %>% 
                                        mutate(p_adj = p.adjust(`Pr..Chisq.`, adj_method)), 
                                      error = function(e) coef_lst$error)
    
    if(.errors) {
      
      return(lrt_result_lst)
      
    } else {
      
      return(lrt_result_lst$result)
      
    }
    
  }
  
# END ----