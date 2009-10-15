Bvar <-
function(XX,theta=1/3){
  #calculates the variance of k polygonial fuzzy numbers with same levels
  #if necessary just use translator first to assure same alpha levels
  #theta is weight in the def of the bertoluzza metric
  if(length(XX)==1){
   v<-0
   return(v)
   }
 if(length(XX)>=2){
  k<-length(XX)
  sample_mean<-Mmean(XX,0)
  if(nrow(sample_mean)<=1){return(c(NA))}
  temp<-rep(0,k)
   for (i in 1:k){
    temp[i]<-(bertoluzza(XX[[i]],sample_mean,theta,0))^2
    }
  v<-mean(temp)
  return(v)
  } 
}

