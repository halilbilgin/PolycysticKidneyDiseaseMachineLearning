function(scopes, data) {
  features <- list()
  # Proteinuri is related to decline in GFR : https://www.researchgate.net/profile/Shigeko_Hara/publication/5353745_Slower_Decline_of_Glomerular_Filtration_Rate_in_the_Japanese_General_Population_A_Longitudinal_10-Year_Follow-Up_Study/links/53eccd530cf2981ada10eb45/Slower-Decline-of-Glomerular-Filtration-Rate-in-the-Japanese-General-Population-A-Longitudinal-10-Year-Follow-Up-Study.pdf
  fit <- glm(as.factor(gfrloss2)~proteinuri, data=db, family=binomial, control=glm.control(maxit=10000))

  for(scope in scopes) {
    step <- stepAIC(fit, scope, direction="both", steps = 100, trace=F)
    features <- append(features, list(step$formula))
  }
 
  names(features) <- names(scopes)
  
  return(features)
}