function(trData, fm, 
         tuneGrid = expand.grid(.mincriterion = 0.99, 
                                            .maxdepth = as.integer(seq(5, 20, 2))),
         controls = ctree_control(teststat = 'max', 
                                  testtype='Teststatistic'), method='ctree2', ...) {
  fit1 <- train(fm, data = trData, 
                method = 'ctree2', 
                tuneGrid = tuneGrid, controls = controls, ...)
  
  return(fit1)
}