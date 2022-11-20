library(dplyr)

#import test data
data("swiss")
dat = swiss

linreg <- function(formula, data){

  #extract inputs
  outcome = formula[[2]]
  outcome = as.character(outcome)
  predictor = formula[[3]]
  predictor = as.character(predictor)

  #Start math
  col = data %>% select(outcome)
  y = matrix(col[[1]], nrow = nrow(data))
  X = cbind(1, as.matrix(x = data[,-1]))
  colnames(X)[1] <- "(Intercept)"

  #linear algebra: n x k matrix
  n = nrow(X)
  k = ncol(X) - 1  #number of predictor variables

  #estimated coeff
  beta = solve(t(X)%*%X)%*%t(X)%*%y

  #var of residuals
  sig_sq = residual_var <- (n-k-1)^-1 * sum((y - X %*% beta)^2)
  residual_se = sqrt(residual_var)

  #var and SE of coefficients
  var_beta <- sig_sq * solve(t(X) %*% X)
  coeff_se <- sqrt(diag(var_beta))

  #test stats
  t_vals <- beta / coeff_se
  p_vals_t <- 2 * pt(abs(t_vals), n-k, lower.tail = FALSE)

  #matching asterisks with sig levels
  aterisks <- function(x){
    ifelse(x <= 0.001,"***",
           ifelse(x <= 0.01,"**",
                  ifelse(x < 0.05,"*",
                         ifelse(x < 0.1,"."," "))))
  }
  sig_levels = sapply(p_vals_t, aterisks)

  #model fitting
  R2 = 1 - (n-k-1)*residual_var / (n*mean((y - mean(y))^2))
  Adj_R2 = 1 - residual_var / ((n/(n-1))*mean((y - mean(y))^2))

  #residuals
  RSS = (n-k-1)*residual_var
  RSS0 = TSS = sum((y - mean(y))^2)

  #F test statistic
  F_stat = ((RSS0 - RSS)/k) / (RSS/(n-k-1))
  p_vals_F = pf(F_stat, df1 = k, df2 = n-k-1, lower.tail = FALSE)

  ##put it all together for output
  #round each individually before throwing into results
  beta = round(beta,5)
  coeff_se = round(coeff_se,5)
  t_vals = round(t_vals,5)
  p_vals_t = round(p_vals_t,5)

  results = as.data.frame(cbind(beta, coeff_se,
                                t_vals, p_vals_t, sig_levels))

  colnames(results) = c("Estimate","Std. Error","t value","Pr(>|t|)","")

  #FINAL OUTPUT
  cat("Coefficients:")
  cat("\n")
  print(results)
  cat("---")
  cat("\n")
  cat("Signif. codes: ",0,"'***'",0.001,"'**'",0.01,"'*'",0.05,"'.'",0.1,"''",1)
  cat("\n")
  cat("\n")
  cat("Residual standard error: ",
      round(residual_se, digits = 3),
      " on ",n-k-1," degrees of freedom",
      "\nMultiple R-squared: ",R2,", Adjusted R-squared: ",Adj_R2,
      "\nF-statistic: ",F_stat, " on ",k-1," and ",n-k-1,
      " DF, p-value: ", p_vals_F,"\n")
}

#testing
linreg(formula = Fertility ~ ., data = dat)

lm(formula = Fertility ~ ., data = dat)
summary(lm(formula = Fertility ~ ., data = dat))


