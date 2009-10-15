sc_mult <-
function(X,b){
 #calculates scalar multiplication of polygonial fuzzy number X with scalar b
 #X has to be in the same format as the output of the translator function
 ok<-checking(X)
 if(ok==0){return(c(NA,NA))}
  nl<-nrow(X)
  leftX<-X[1:(nl/2),]
  temp<-X[(nl/2+1):nrow(X),]
  rightX<-temp[nrow(temp):1,]
  if(b>=0){
   sc<-X
   sc$x<-b*X$x
   E<-sc
  }
  if(b<0){
   sc<-X
   temp<-b*X$x
   sc$x<-temp[length(temp):1]
   E<-sc
  }
 return(E)
}

