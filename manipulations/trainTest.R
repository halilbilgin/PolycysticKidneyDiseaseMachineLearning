function(dataset, p = 0.75) {
  dataset[, 'train'] <- ifelse(runif(nrow(dataset)) <= p, 1, 0)
  
  trData <- dataset[dataset[, 'train'] == 1,]
  tData <- dataset[dataset[, 'train'] == 0,]
  trData[,'train'] <- NULL
  
  tData[,'train'] <- NULL

  return(list(train=trData, test=tData))
}
