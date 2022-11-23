library(dplyr)

linreg <- function(formula, data){

  #extract outcome variable name from input
  outcome = formula[[2]]
  outcome = as.character(outcome)

  #extract name of predictor variable(s) based on input
  #account for multiple predictors
  if(formula[[3]][[1]] == "+"){
    if(length(formula[[3]]==3 & length(formula[[3]][[2]])==1)){
      predictor = c(as.character(formula[[3]][[2]]), as.character(formula[[3]][[3]]))
    }
    if(length(formula[[3]]==3 & length(formula[[3]][[2]][[2]])==1)){
      predictor = c(as.character(formula[[3]][[2]][[2]]), as.character(formula[[3]][[3]]))
    }
    if(length(formula[[3]]==3 & length(formula[[3]][[2]][[2]][[2]])==1)){
      predictor = c(as.character(formula[[3]][[2]][[2]][[2]]), as.character(formula[[3]][[2]][[2:3]]),
                    as.character(formula[[3]][[2]][[3]]), as.character(formula[[3]][[3]]))
    }
    if(length(formula[[3]]==3)){
      predictor = c(as.character(formula[[3]][[2]][[2]][[2]][[2]]), as.character(formula[[3]][[2]][[2]][[2:3]]),
                    as.character(formula[[3]][[2]][[2:3]]), as.character(formula[[3]][[2]][[3]]),
                    as.character(formula[[3]][[3]]))
    }
  }else{
    predictor = formula[[3]]
  }
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
