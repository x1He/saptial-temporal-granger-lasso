#INPUTS:
# 'series': A NxTxS matrix 
# 'L':  	Length of the lag
# 'lambda': 	Value of the penalization parameter in Lasso
# 'type': 	'l' Lasso with the given Lambda
#  require r package glmnet
# based on the matlab version 

#spatial-temporal granger lasso
grangerlasso <-function(series, L, lamda2, gama)
{
  N = dim(series)[1]
  T = dim(series)[2]
  S = dim(series)[3]
  
  #delta j can be I or exL(||w||/2)
  delta = array(NA,c(T*S,T*S,N)
  delta_S = array(NA,c(T*S,T*S,N)
  A = array(NA,c(T*S,T*S,N)  
            
  for(j in 1:N){
    delta[,,j] = diag(T*S)
    delta_S[,,j] = chol(delta[j])
    A[,,j] = solve(delta_S[,,j])
  }
  
  C = A[,,1]
  for(i in 2:N){
    C = bdiag(C,A[,,i])
  }
 
  #group lasso beta
  GL_beta
  
  p = N*L*S
  q = (T-L+1)*S
  Y = matrix(0,q,1)
  X = matrix(0,q,p)
  
  append2Y = matrix(0,p,1)
  append2X = sqrt(lamda2)*diag(p)
  
  newY = rbind(Y,append2Y)
  newX = (1/sqrt(1+lamda2))*rbind(X,append2X)
  
  
  
  
  
#   for (i in (L+1):T){
#     bm[i-L] = series[1, i];
#     Am[i-L,] = matrix(series[,(i-1):(i-L)],1,N*L)
#   }
 
  fit = glmnet( Am, bm, lambda=lmd,family="gaussian",alpha=1)
  vals2 = fit$beta
  
  n1Coeff = matrix(0,N,L)
 
  for (i in 1:N){
    n1Coeff[i,] = vals2[((i-1)*L+1): (i*L)]
  }
 
  sumCause = matrix(rowSums(abs(n1Coeff)),N,1)
  cause = (sumCause > 0)*sumCause;
  
  return(cause)
  
}