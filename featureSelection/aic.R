function(scopes, data) {
  features <- list()
  fit <- glm(as.factor(gfrloss2)~1, data=db, family=binomial, control=glm.control(maxit=10000))

  for(scope in scopes) {
    step <- stepAIC(fit, scope, direction="both", steps = 1000, trace=F)
    features <- append(features, list(step$formula))
  }
 
  names(features) <- names(scopes)
  
  return(features)
}