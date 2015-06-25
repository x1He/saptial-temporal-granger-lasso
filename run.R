# run grangerlasso.R first
# input matrix

#spatial-temporal granger lasso
refinedtest = test[,c(4:8,10:21)]
input = t(refinedtest)
#Y is a vector of q (time,latitude,longtitude). To get the vector for a varibale such as ndvi, 
#sort year in order(start from L), then latitude in order, then longtitude in order to get the vector.
Y =
  
# X is matrix with q X p, p is in form of (variable,lag,(latitude,longtitue)+(w1,w2)),each in order
X = 

#
T =
N = dim(input)[1]
L =
S = 
omega = 

  
p = N*L*omega
q = (T-L+1)*S
Y = matrix(0,q,1)
X = matrix(0,q,p)

#find (lamda,gama) pair min bic
lamda2set = c(0,0.01,0.1,1,10,100)
gama_max = 
gamaset = c(0,0.01*gama_max,0.1*gama_max,gama_max)

lamdasize = length(lamda2set)
gamasize = length(gamaset)
#lamda,gamma combination array
#res = array(NA,c(1,2,1))
#bic value range 
#minbic = 0
cur = 1 
for(i in 1:lamdasize){
  for(j in 1:gamasize){
    betaGL = grangerlasso(Y,X,gamaset[j],T,N,omega)
    dfgl = dfGL(lamda,gama,betam,N,p,X,Y)
    bicvalue = bic(betaGL, x, y,dfgl)
    #res[,,cur] = c(lamda2set[i],gamaset[j])
    #cur = cur + 1
    if(i==1 && j==1){
      minbic = bicvalue
      res = c(lamda2set[i],gamaset[j])
    }
    else{
      minbic = min(minbic,bicvalue)
      if(minbic == bicvalue){
        res = c(lamda2set[i],gamaset[j])
      }
    }
  }
}





