library(missForest)
function(dataraw, delOutliers=T) {
  dataraw[, c('gozlemno', 'CVolay', 'gfrloss', 'diyaliz', 'diyaliz_sure')] <-NULL
  set.seed(seed)
  dataimp = missForest(dataraw)$ximp
  
  if(delOutliers) {
    dataimp <- manipulations$deleteOutliers(dataimp, 'LDL') 
    dataimp[, 'kolesterol'] <- ifelse(dataimp[,'kolesterol'] > 550, NA, 
                                      dataimp[,'kolesterol'])
    
    dataimp[,'crp'] <- ifelse(dataimp[,'crp'] > 150, NA, dataimp[,'crp'])

    dataimp <- missForest(dataimp)$ximp  
  }
  
  dataimp[, 'cinsiyet'] <- ifelse(dataimp[,'cinsiyet'] == 'erkek', 0, 1)
  dataimp[, 'ht'] <- ifelse(dataimp[, 'ht'] == 'var', 1, 0)
  dataimp[, 'gfrloss2'] <- as.factor(dataimp[,'gfrloss2'])
  dataimp[, 'yas40'] <- ifelse(dataimp$yas < 40, 1, 0)
  return(dataimp)
}