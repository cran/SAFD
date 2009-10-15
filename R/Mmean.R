Mmean <-
function(XX,pic=0){
  #calculates the Minkowski mean of k=length(XX) polygonial fuzzy numbers with same levels
  #if necessary just use translator first to assure same alpha levels
  k<-length(XX)
  X1<-Msum(XX)
  if(nrow(X1)==1){
   X2<-X1
   return(X2)
   }
   
   X2<-sc_mult(X1,1/k)
  
   #start possible plotting---------------------------------------------------
   if(pic==1){
      #calculate plot limits:
       lower<-rep(0,k)
       upper<-lower
        for (j in 1:k){
         lower[j]<-min(XX[[j]])
         upper[j]<-max(XX[[j]])
        }
       limx<-c(min(lower),max(upper))
     plot(X2,type="l", xlim=limx,lwd=2,xlab="x", ylab="alpha",cex.main=1, col="red",
          main=paste("Sample and sample mean (red)",sep=""))
     for (j in 1:k){
      lines(XX[[j]],type="l",lwd=0.3,col="black")
      }
     lines(X2,type="l", lwd=2,col="red") 
    }
   #end possible plotting------------------------------------------------------
  return(X2)
}

