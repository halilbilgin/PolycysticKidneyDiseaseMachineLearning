function(scopes, data) {
  features <- list()
  library(glmnet, verbose = F)
  set.seed(seed)

  
  return(sapply(scopes, function(scope) {
    MSEs <- NULL
    
    cv <- cv.glmnet(model.matrix(scope, data)[,-1], data$gfrloss2, 
                    family='binomial', alpha=1, nfolds=10)
    coefs <- as.matrix(predict(cv, type='coefficients', s=cv$lambda.min))
    coefs <- coefs[coefs[,'1'] != 0,]
    coefs <- names(coefs)[names(coefs) != '(Intercept)']
    if(length(coefs) == 0){
      coefs <- c(1)
    }
    return(as.formula(paste("as.factor(gfrloss2) ~ ", paste(coefs, collapse = '+'))))
    }))
  
}