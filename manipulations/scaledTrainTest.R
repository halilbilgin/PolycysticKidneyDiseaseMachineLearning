function(trData, tData) {

  scaledtrain <- as.data.frame(lapply(trData, function(x) rep.int(NA, length(x))))
  scaledtest <- as.data.frame(lapply(tData, function(x) rep.int(NA, length(x))))
  
  for(feature in colnames(trData)) {
    x <- trData[, feature]
    if(is.numeric(x) && ! (feature %in% c('ht', 'cinsiyet')) ){
      newRow <- c(var(x), mean(x))
      
      scaledtrain[, feature] <- (trData[, feature] - newRow[2]) / newRow[1]
      scaledtest[, feature] <- (tData[, feature] - newRow[2]) / newRow[1]
    } else {
      scaledtrain[, feature] <- trData[, feature]
      scaledtest[, feature] <- tData[, feature]
    }
    
    
  }
  return(list(train=scaledtrain, test=scaledtest))
}
