function(scopes, data) {
  features <- list()
  library(glmnet, verbose = F)
  set.seed(seed)

  
  return(sapply(scopes, function(scope) {
    MSEs <- NULL
    
    cv <- cv.glmnet(model.matrix(gfrloss2~., scaledDb)[,-1], scaledDb$gfrloss2, 
                    family='binomial', alpha=1, nfolds=5)
    
    coefs <- as.matrix(predict(cv, type='coefficients', s=cv$lambda.min))
    coefs <- coefs[coefs[,'1'] != 0,]
    coefs <- names(coefs)[names(coefs) != '(Intercept)']
    
    return(as.formula(paste('as.factor(gfrloss2) ~ ', paste(coefs, collapse = '+'))))
    }))
  
}