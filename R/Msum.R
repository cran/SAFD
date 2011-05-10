Msum <-
function(XX){
  #calculates the Minkowski sum of k polygonial fuzzy numbers with same levels
  #if necessary just use translator first to assure same alpha levels
  #if check is not okay or if sample elements have different alpha levels NA is returned
  k<-length(XX)
  temp<-rep(0,k)
  for (i in 1:k){
   temp[i]<-checking(XX[[i]],0)
  }
  ok<-1
  if(min(temp)==0){
   print(paste("One or more elements of the sample don't define a polygonal fuzzy number"))
   print(paste("or are not in the correct format"))
   ok<-0
   }
  if(min(temp)==1){
   if(length(XX)==1){R<-XX[[1]]}
   if(length(XX)>=2){
    number<-rep(0,k)
    for (i in 1:k){
     number[i]<-nrow(XX[[i]])
     }
    if(max(number)!=min(number)){
     print("use translator function to assure that list elements are compatibel (same alpha levels)")
     ok<-0
     }
    if(max(number)==min(number)){
     equal<-rep(0,k-1)
      for (i in 1:(k-1)){
      equal[i]<-max(abs(XX[[1]]$alpha-XX[[i]]$alpha))
      }
     if(max(equal)>0){
      print("use translator function to assure that list elements are compatibel (same alpha levels)")
      ok<-0
     }
     }
    }
    if(length(XX)>=2&ok==1){
      R<-XX[[1]]
       for (i in 2:k){
        R$x<-R$x+XX[[i]]$x
       }
    }
   }
   if(ok==1){
  invisible(R)
  }
}

