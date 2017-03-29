function(trData, fm, method="rf", mtry = sqrt(ncol(trData) - 2), 
         tuneGrid = expand.grid(.mtry=mtry), ...) {

  fit <- train(fm, data=trData, method=method,  tuneGrid=tuneGrid, ...)
  
  return(fit)
}