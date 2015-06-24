dfGL <- function(lamda,gama,betam,N,p,X,Y)
{
  #beta = array(NA,c(p/N,p/N,N)
  beta = array(NA,c(p/N,1,N))
  b_ols = array(NA,c(p/N,1,N))
  sum = 0
  beta_OLS = solve(t(X)%*%X)%*%t(X)%*%Y
  
  for(j in 1:N){
    beta[,j] = betam[((j-1)*p/N+1):(j*p/N),]
    b_ols[,j] = beta_OLS[((j-1)*p/N+1):(j*p/N),]
    sum = sum + diag(p/N)%*%(sqrt(max(eigen(t(beta[,j])%*%beta[,j])$values)) > 0) + norm(beta[,j],"1")/norm(b_ols[,j],"1")
  }
  
  return(sum)
}