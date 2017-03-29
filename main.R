source('./autoload.R', chdir=T)



#####################
#### MODEL COMPARISON
#####################

## Models to be compared
algorithms <- loadClassifiers(c('knn', 'rf', 'svmRadial', 'glm', 'cforest', 'ctree'))

## Train control..
set.seed(seed)
tt <- createDataPartition(db$gfrloss2, p=0.75, times=30)
control <- trainControl(method="repeatedcv", number=10, repeats=3, index=tt)


## Feature selection with AIC
scopes <- c(featureSelection$allCombinations(db,F), featureSelection$allCombinations(db,T,T))
names(scopes) <- c('AIC(ALL)', 'AIC(All+SecondPower+Log)')

features <- featureSelection$aic(scopes, db)

## Adding all features without selections.
features <- append(features, list(as.factor(gfrloss2)~.))
names(features)[3] <- "ALL"


allFits <- list()

## creating a list that will contain fits
for(feature in features) {
  fits <- list()
  # Run all algorithms for selected feature set
  
    for(i in 1:length(algorithms)) { 
    set.seed(seed)
    
    methodName <- names(algorithms)[i]
    classifier <- algorithms[[i]]
    fit <- classifier(trData=db, fm=feature,
                      method=methodName, metric='Accuracy',
                      trControl=control, tuneLength=20)
    
    fits <- append(fits, list(fit))
    }
  names(fits) <- names(algorithms)
  
  allFits <- append(allFits, list(fits))
}

names(allFits) <- names(features)

for(i in 1:length(allFits)) {
  
  fits <- allFits[[i]]
  featureName <- names(allFits)[i]
  
  print(featureName)
  print(features[[i]])
  
  ## Get results for each resample 
  resampleResults <- resamples(fits, decreasing=T, metric='Accuracy')
  # box and whisker plots to compare models
  scales <- list(x=list(relation="free"), y=list(relation="free"))

  
  print(bwplot(resampleResults, scales=scales, main=featureName))
  
  diffs <- diff(resampleResults)
  # summarize p-values for pair-wise comparisons
  print(summary(diffs))
}

##################
### Predictions 
##################
truths <- c()
predictions <- c()

for(i in 1:10) {
  tt <- manipulations$trainTest(db, p=0.9)
  trainCtrl <- trainControl(method="repeatedcv", number=10, repeats=3)
  fit <- algorithms$svmRadial(tt$train, features$`AIC(All+SecondPower+Log)`, trControl=trainCtrl)
  predictions <- c(predictions, predict(fit, newdata=tt$test))
  truths <- c(truths, tt$test$gfrloss2)
}

confusionMatrix(predictions, truths)

##################
##CORRELATIONS
##################
library(qtlcharts)

selectedCols <- sapply(db, function(x) {is.numeric(x)})

iplotCorr(scaledDb[, selectedCols],
          scatterplots = T, corr = cor(scaledDb[, selectedCols], method = c("pearson", "kendall", "spearman")))
?iplotCorr
