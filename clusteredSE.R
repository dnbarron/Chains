get_CL_vcov <- function(model, cluster){
  
  # Function to calculate clustered standard errors
  # Assumes that the clustering variable comes from data frame
  # of same size (probably best if actually a variable in)
  # as data used in estimation. 
  
  equire(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  
  # If NAs were removed by the estimation function, remove the same
  # rows from the cluster id variable
  
  if (length(model$na.action) > 0) {
    cluster <- cluster[-model$na.action]
  }
  
  #calculate degree of freedom adjustment
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- length(coef(model))
  dfc <- (M / (M - 1)) * ((N - 1) / (N - K))
  
  #calculate the uj's
  uj  <- apply(estfun(model),2, function(x) tapply(x, cluster, sum))
  
  #use sandwich to get the var-covar matrix
  vcovCL <- dfc * sandwich(model, meat = crossprod(uj) / N)
  vcovCL
}

get_confint<-function(model, vcovCL){
  
  # Calculates confidence intervals using supplied vcov matrix
  
  t <- qt(.975,  model$df.residual)
  ct <- coeftest(model, vcovCL)
  est <- cbind(ct[, 1], ct[, 1] - t * ct[, 2], ct[, 1] + t * ct[, 2])
  colnames(est) <- c("Estimate","LowerCI","UpperCI")
  return(est)
}
