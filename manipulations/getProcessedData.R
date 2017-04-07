library(missForest)
function(dataraw, delOutliers=T) {
  dataraw[, c('gozlemno', 'CVolay', 'gfrloss', 'gfr', 'diyaliz', 'diyaliz_sure')] <-NULL
  set.seed(seed)
  dataimp = missForest(dataraw)$ximp
  
  if(delOutliers) {
    dataimp <- manipulations$deleteOutliers(dataimp, 'LDL') 
    dataimp <- manipulations$deleteOutliers(dataimp, 'kolesterol')
    
    dataimp[,'crp'] <- ifelse(dataimp[,'crp'] > 150, NA, dataimp[,'crp'])

    dataimp <- missForest(dataimp)$ximp  
  }
  
  dataimp[, 'cinsiyet'] <- ifelse(dataimp[,'cinsiyet'] == 'erkek', 0, 1)
  dataimp[, 'ht'] <- ifelse(dataimp[, 'ht'] == 'var', 1, 0)
  dataimp[, 'gfrloss2'] <- as.factor(dataimp[,'gfrloss2'])

  return(dataimp)
}