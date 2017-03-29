function(dataset) {
  return(as.data.frame(sapply(colnames(dataset), function(x) {
    if(is.numeric(db[, x]) && !(x %in% c('ht', 'cinsiyet','gfrloss2'))) {
      output <- scale(db[, x], center = TRUE, scale = TRUE)
    } else {
      output <- db[, x]
    }
    return(output)
    
  }))) 
}
