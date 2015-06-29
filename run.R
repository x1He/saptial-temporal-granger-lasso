# run grangerlasso.R first
#spatial-temporal granger lasso

#Y is a vector of q (time,latitude,longtitude). To get the vector for a varibale such as ndvi, 
#sort year in order(start from L), then latitude in order, then longtitude in order to get the vector.
# example: NDVI      
#       latitude longtitude ndvi(Y)
# 1981
# 1981
#   :
#   :
# 2014
# 2014
Y =
  
# X is matrix with q X p, p is in form of (variable,lag,(latitude,longtitue)+(w1,w2)),each in order
# X table
#   (variable1,lag1,(latitude,longtitue)+(w1,w2)).... (variableN,lag1,(latitude,longtitue)+(w1,w2))....
# Y
# :
# :
X = 

# T time length(34 if from 1981-2014), N varible numbers, L time lags, S location point, omega
# nearby point (-2,-1,0,1,2) X (-2,-1,0,1,2)
T =
N = 
L =
S = 
omega = 

  
p = N*L*omega
q = (T-L+1)*S
Y = matrix(0,q,1)
X = matrix(0,q,p)

#find (lamda,gamma) pair min bic
lamda2set = c(0,0.01,0.1,1,10,100)
gamma_max = 
gammaset = c(0,0.01*gamma_max,0.1*gamma_max,gamma_max)

lamdasize = length(lamda2set)
gammasize = length(gammaset)

for(i in 1:lamdasize){
  for(j in 1:gammasize){
    betaGL = grangerlasso(Y,X,lamda2set[i],gammaset[j],T,N,L,omega)
    dfgl = dfGL(lamda,gamma,betaGL,N,p,X,Y)
    bicvalue = bic(betaGL,X,Y,dfgl)
    
    if(i==1 && j==1){
      minbic = bicvalue
      res = c(lamda2set[i],gammaset[j])
    }
    else{
      minbic = min(minbic,bicvalue)
      if(minbic == bicvalue){
        res = c(lamda2set[i],gammaset[j])
      }
    }
  }
}





