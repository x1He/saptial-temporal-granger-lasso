bic <-function(beta, x, y,dfGL)
{
  sigma = 
  n = 
  M = y - x%*%beta
  BIC = t(M) %*% M / (n*sigma*sigma) + log(n)/n*dfGL
  return(BIC)
}