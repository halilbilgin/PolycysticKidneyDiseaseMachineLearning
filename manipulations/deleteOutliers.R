function(dt, var) {
  var_name <- dt[,var]
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  
  dt[, var] <- invisible(var_name)

  return(dt)
}