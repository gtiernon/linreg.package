library(dplyr)

linreg <- function(formula, data){

  #extract inputs
  outcome = formula[[2]]
  outcome = as.character(outcome)
  predictor = formula[[3]]
  predictor = as.character(predictor)

  #set up matrices
  out = data %>% select(outcome)

  if(predictor=="."){
    pred = data %>% select(-outcome)
  }else{
    pred = data %>% select(predictor)
  }

  y = matrix(out[[1]], nrow = nrow(data))
  X = cbind(1, as.matrix(x = pred))
  colnames(X)[1] <- "(Intercept)"

  #estimated coeff
  beta = solve(t(X)%*%X)%*%t(X)%*%y

  #final result as a list to replicate lm()
  output = list("Call" = match.call(), "Coefficients" = t(round(beta,4)))

  return(output)
}
