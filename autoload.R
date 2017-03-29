## Auto Load scripts.. 
## Halil Bilgin
library(glmnet, verbose = F, quietly = T)
library(caret, quietly = T)
library(MASS, quietly = T)

dirs <- c('classifiers', 'manipulations', 'featureSelection')

for(dirName in dirs) {
  assign(dirName, list())
}


customLoader <- function () {
  for(filename in list.files(dirs, full.names = T)) {
    
    dirName <- dirname(filename)
    scriptList <- get(dirName)
    
    script <- source(filename, chdir=T)$value
    
    assign(dirName, append(scriptList, list(script)), envir=globalenv())
  }
  
  for(dirName in dirs) {
    scriptList <- get(dirName)
    
    names(scriptList) <- lapply(list.files(dirName),function(x) {
      # remove .R

      x <- gsub('.R', '', x)
      # remove my.
      gsub('my.', '', x) 
    })
    assign(dirName, scriptList, envir=globalenv())
  }
}

customLoader()

loadClassifiers <- function(clNames) {
  result <- list()
  for(clName in clNames) {
    fn<-''
    if(! (clName %in% names(classifiers))) {
      
      fn <- function (trData, fm, method=clName, ...) {
     
        return(train(fm, data=trData,method=method,...));
      }
    } else {
      fn <- classifiers[[clName]]
    }
    
    result <- append(result, list(fn))
  }
  names(result) <- clNames
  return(result)
}

remove(dirs, dirName, customLoader)

## Load the data if not loaded
if(! exists('db'))
{
  db <- manipulations$getProcessedData(read.table(file = "polikistikML.txt", header = T))
  scaledDb <- manipulations$scaleDb(db)
}
## seed constant 
seed <- 7