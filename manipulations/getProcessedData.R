library(missForest)
function(dataraw) {
  dataraw[, c('gfrloss', 'gfr', 'CVolay', 'gozlemno', 'diyaliz', 'diyaliz_sure')] <-NULL
  dataraw <- dataraw[dataraw[, 'crp'] < 150,] 

  dataimp = missForest(dataraw)$ximp
  dataimp[, 'eGFR'] <- 186 * (dataimp$kreatinin / 88.4)^(-1.154) * (dataimp$yas)^(-0.203) * ifelse(dataimp[,'cinsiyet'] == 'kadın', 0.742, 1) 
  for(nm in names(dataimp)) {
    if(is.numeric(dataimp[,nm])) {
      dataimp <- manipulations$deleteOutliers(dataimp, nm) 
    }
  }
  dataimp <- missForest(dataimp)$ximp
  
  dataimp[, 'cinsiyet'] <- ifelse(dataimp[,'cinsiyet'] == 'erkek', 0, 1)
  dataimp[, 'ht'] <- ifelse(dataimp[, 'ht'] == 'var', 1, 0)
  dataimp[, 'gfrloss2'] <- as.factor(dataimp[,'gfrloss2'])

  return(dataimp)
}