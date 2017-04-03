function(trData, fm, method='', ...) {
  nb <- list(type = "Classification",
                library = "naivebayes",
                loop = NULL)
  
  nb$parameters <-  data.frame(parameter = "parameter",
                               class = "character",
                               label = "parameter")
  
  nb$grid <- function(x, y, len = NULL, search = "grid") 
    data.frame(parameter = "none")
  
  nbProd <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type="probabilities")
  nb$prob <- nbProd
  
  nbFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
    naive_bayes(x = as.matrix(x), y = y,
         ...)
  }
  nb$fit <- nbFit
  
  nbPred <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit, newdata)
  }
    
  nb$predict <- nbPred
  nbSort <- function(x) x
  nb$sort <- nbSort
  nb$levels <- function(x) levels(x@data@get("response")[,1])
    return (train(fm, data = trData, 
                method = nb, ...))
}