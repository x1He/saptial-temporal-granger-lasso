#INPUTS:
#Y: vector with length q, each Y can be a varibale such as ndvi or temperature
#x: qxp matrix
# based on the matlab version 

#spatial-temporal granger lasso
grangerlasso <-function(Y,X,gama,T,N,omega)
{
  #delta j can be I or exL(||w||/2)
  delta = array(NA,c(L*omega,L*omega,N)
  delta_S = array(NA,c(L*omega,L*omega,N)
  A = array(NA,c(L*omega,L*omega,N)  
            
  for(j in 1:N){
    delta[,,j] = diag(L*omega)
    delta_S[,,j] = chol(delta[j])
    A[,,j] = solve(delta_S[,,j])
  }
  
  C = A[,,1]
  for(i in 2:N){
    C = bdiag(C,A[,,i])
  }
  
  p = N*L*omega
  q = (T-L+1)*S
  append2Y = matrix(0,p,1)
  append2X = sqrt(lamda2)*diag(p)
  
  newY = rbind(Y,append2Y)
  newX = (1/sqrt(1+lamda2))*rbind(X,append2X)
   
  fit = glmnet( newX, newY, lambda=gama,family="gaussian",alpha=1)
  vals2 = fit$beta
  
  return(vals2)
#   n1Coeff = matrix(0,N,L)
#  
#   for (i in 1:N){
#     n1Coeff[i,] = vals2[((i-1)*L+1): (i*L)]
#   }
#  
#   sumCause = matrix(rowSums(abs(n1Coeff)),N,1)
#   cause = (sumCause > 0)*sumCause;
#   
#   return(cause)
  
}