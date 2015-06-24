# run grangerlasso.R first
# input matrix

#spatial-temporal granger lasso
refinedtest = test[,c(4:8,10:21)]
input = t(refinedtest)
N = dim(input)[1]
L = 

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
    betaGL = beta(input,gama,L)
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





