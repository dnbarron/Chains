ef.plot <- function(data, ..., .model){
  require(modelr, quietly = TRUE)
  require(ggplot2, quietly = TRUE)
  
  vars <- list(...)
  
  print(unlist(vars))
  
  new_data <- data_grid(data = data, vars, .model = .model)
  pred <- predict(.model, newdata = new_data)
  ret <- cbind(new_data, pred)
  ret <- gather(ret, 'Rating', 'Prediction', starts_with('fit.'))
  ret$Rating <- stringr::str_sub(ret$Rating, 5)
  #  ret$Rating <- ordered(ret$Rating, levels = c('Inadequate', 'Poor', 'Good'))
  
  xvar <- vars[[1]]
  if (length(vars) == 1) {
    zvar <- NULL
  } else {
    zvar <- vars[[2]]
  }  
  
  g <- ggplot(ret, aes_(x = substitute(xvar), y = quote(Prediction), 
                        group = substitute(zvar), colour = substitute(zvar))) + 
    geom_line() +
    facet_grid(~ Rating) + 
    theme(legend.position = 'top') 
  
  if (dim(ret)[1] < 100){
    print(g + geom_point())
  } else print(g)  
  
  invisible(ret)
}

clm_estimates <- function(f = forms.new, data = comp12.noLA, chn = 'log_chain_size'){
  forms <- map(f, ~ add_predictors(formula(.x, env = .GlobalEnv), formula(paste('~ ', chn))))
  fit <- fit_with(data, clm, forms, Hess = TRUE) 
  hc <- map(fit, ~ get_CL_vcov(.x, comp12.noLA$care.home.group.new))
  return(list(fit = fit, formula = forms, tidyop = map2(fit, hc, ~tidy(.x, cluster = TRUE, vcov = .y))))
}

ef.plot(comp12.noLA, "in_chain", "sector", .model = fit.di.int[[5]])