library(missForest)
function(dataraw) {
  dataraw[, c('gfrloss', 'gfr', 'CVolay', 'gozlemno', 'diyaliz', 'diyaliz_sure')] <-NULL
  
  dataraw <- dataraw[dataraw[, 'crp'] < 150,] 
  dataimp = missForest(dataraw)$ximp
  dataimp[, 'cinsiyet'] <- ifelse(dataimp[,'cinsiyet'] == 'erkek', 0, 1)
  dataimp[, 'ht'] <- ifelse(dataimp[, 'ht'] == 'var', 1, 0)
  dataimp[, 'gfrloss2'] <- as.factor(dataimp[,'gfrloss2'])
  return(dataimp)
}