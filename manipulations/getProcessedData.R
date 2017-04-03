library(missForest)
function(dataraw, delOutliers=T) {
  dataraw[, c('gozlemno', 'CVolay', 'gfrloss', 'gfr', 'diyaliz', 'diyaliz_sure')] <-NULL
  
  dataimp = missForest(dataraw)$ximp
  #dataimp[, 'eGFR'] <- 186 * (dataimp$kreatinin / 88.4)^(-1.154) * (dataimp$yas)^(-0.203) * ifelse(dataimp[,'cinsiyet'] == 'kadın', 0.742, 1) 
  
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