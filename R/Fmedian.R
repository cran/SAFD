Fmedian <-
function(XX,pic=1){
 k<-length(XX)
 m<-Mmean(XX,pic=0)
 if(is.null(m)==0){
  Q<-Fquantile(XX,p=c(0.5))[[1]]
  if(pic==1){
    #calculate plot limits:
    lower<-rep(0,k)
    upper<-lower
    for (j in 1:k){
      lower[j]<-min(XX[[j]])
      upper[j]<-max(XX[[j]])
     }
    limx<-c(min(lower),max(upper))
     plot(Q,type="l", xlim=limx,lwd=2,xlab="x", ylab="alpha",cex.main=1, col="red",
          main=paste("Sample, sample mean (red) and sample median (blue)",sep=""))
     for (j in 1:k){
      lines(XX[[j]],type="l",lwd=0.3,col="black")
      }
     lines(m,type="l", lwd=2,col="red")
     lines(Q,type="l", lwd=2,col="blue")
    }
   #end possible plotting---------
  invisible(Q)
 }
}
