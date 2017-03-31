function(db, includeSecondPower=F, includeLog=F, exclude=c()) {
  fm <- "as.factor(gfrloss2) ~ "
  prNames <- names(db[,-which(names(db) == 'gfrloss2')])
  for(i in 1:length(prNames)) {
    if(prNames[i] %in% exclude) {
      next
    }
    
    fm <- paste(fm, prNames[i], '+')
    
    if(prNames[i] %in% c('cinsiyet', 'ht')) {
      next
    }
    if(includeSecondPower){
      fm <- paste(fm, paste('I(',prNames[i],'^2)', sep=""), '+')  
    }
    if(includeLog) {
      fm <- paste(fm, paste('log(',prNames[i],')', sep=""), '+')  
    }
    
  }
  
  
  fm <- as.formula(substr(fm, 1, nchar(fm)-1))
  return(fm)
}