function(db, includeSecondPower=T, includeLog=F) {
  fm <- "as.factor(gfrloss2) ~ "
  prNames <- names(db[,-which(names(db) == 'gfrloss2')])
  for(i in 1:length(prNames)) {
    if(prNames[i] %in% c('cinsiyet', 'ht')) {
      next
    }
    if(includeSecondPower){
      fm <- paste(fm, paste('I(',prNames[i],'^2)', sep=""), '+')  
    }
    if(includeLog) {
      fm <- paste(fm, paste('log(',prNames[i],')', sep=""), '+')  
    }
    fm <- paste(fm, prNames[i], '+')
    
  }
  fm <- paste(fm, 'cinsiyet+ht')
  fm <- as.formula(fm)
  return(fm)
}