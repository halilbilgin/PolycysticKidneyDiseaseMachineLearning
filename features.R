fm <- featureSelection$allCombinations(db)
## AIC, Stepwise


fit <- glm(gfrloss2~1, data=scaledDb, family=binomial, control=glm.control(maxit=10000))
step <- stepAIC(fit, gfrloss2~., direction="both", steps = 1000)
step$formula
step$anova # display results

#more feature selection with rf 
tt <- createDataPartition(db$gfrloss2, p=0.75, times=30)
control <- trainControl(method="repeatedcv", number=10, repeats=3, index=tt)



scaledTt <- manipulations$trainTest(db)

# Fitting a LogReg with AIC results.

## Confusion Matrix
predictions <- ifelse(predict(fit, newdata=scaledTt$test, type='response')>0.5,'yuksek','az')
confusionMatrix(predictions, scaledTt$test$gfrloss2)


## Fitting a SVM with AIC results
svmfit <- classifiers$svadial(scaledTt$train, gfrloss2~ bmi + urikasit + 
                                fosfor + k + hdl + LDL + glukoz + KBsist)
predictions <- predict(svmfit, newdata=scaledTt$test)
confusionMatrix(predictions, scaledTt$test$gfrloss2)


plot(varImp(fit, scale=T))


?cv.glmnet
