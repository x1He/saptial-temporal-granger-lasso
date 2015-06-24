# run grangerlasso.R first
# input matrix

#spatial-temporal granger lasso
refinedtest = test[,c(4:8,10:21)]
input = t(refinedtest)
N = dim(input)[1]
      

#find (lamda,gama) pair min bic
lamda2set = c(0,0.01,0.1,1,10,100)
gama_max = 
gamaset = c(0,0.01*gama_max,0.1*gama_max,gama_max)




