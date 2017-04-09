function(dataset) {
  return(as.data.frame(sapply(colnames(dataset), function(x) {
    if(is.numeric(dataset[, x]) && !(x %in% c('ht', 'cinsiyet','gfrloss2', 'yas40'))) {
      output <- scale(dataset[, x], center = TRUE, scale = TRUE)
    } else {
      output <- dataset[, x]
    }
    return(output)
    
  }))) 
}
