function(trData, fm, 
         tuneGrid = expand.grid(.mincriterion = 0.95, 
                                            .maxdepth = as.integer(seq(from=2, to=5, by=1))),
         controls = ctree_control(teststat = 'max', 
                                  minsplit=5), method='ctree2', ...) {
  fit1 <- train(fm, data = trData, 
                method = 'ctree2', 
                tuneGrid = tuneGrid, controls = controls, ...)
  
  return(fit1)
}
