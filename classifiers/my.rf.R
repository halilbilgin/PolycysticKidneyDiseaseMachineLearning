function(trData, fm, method="rf", mtry = c(1,2,3,4), 
         tuneGrid = expand.grid(.mtry=mtry), ...) {

  fit <- train(fm, data=trData, method=method,  tuneGrid=tuneGrid, ...)
  
  return(fit)
}