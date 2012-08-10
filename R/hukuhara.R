hukuhara <-
function(X,Y,pic=1){
 #it is checked if the huku diff Y-X exists and if yes it is claculated and returned
 #first check validity of input data
 temp_mean<-Mmean(list(X,Y))
 if(is.null(temp_mean)==0){
  nl<-nrow(X)/2
  dif<-data.frame(x=Y$x-X$x,alpha=X$alpha)
  #calculate for each alpha-level the interval-hukuhara diff and check if this
  #family of intervals is decreasing in alpha ->if yes then return the polygonal fuzzy number that
  #is the hukuhara diff
  if(pic==1){
    plot(X,type="l",xlim=c(min(c(X$x,Y$x)),max(c(X$x,Y$x))))
    lines(Y,type="l",lwd=1.5)
    }
  a<-checking(dif,0)
  if(a==0){
   print("Hukuhara difference Y-X does not exist")
   }
 if(a==1){
  if(pic==1){
   plot(X,type="l",xlim=c(min(c(X$x,Y$x)),max(c(X$x,Y$x))),
      main=paste("Fuzzy numbers and their Hukuhara difference (in red)",sep=""),cex.main=1)
   lines(Y,type="l",lwd=1.5)
   lines(dif,type="l",col="red")
   }
  invisible(dif)
  }
  }
 }
