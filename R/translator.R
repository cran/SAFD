translator <-
function(X,nl=101){
 #X...2-dim data.frame with colums "x" and "alpha" containing the vertexes of the polygonial fuzzy number
 #nl...numer of levels that shall be calculated, uniformly in [0,1], at least 2
 ok<-checking2(X)
 if(nl<=1){
  print("Minimum number of output levels nl is 2")
  }
 if(nl>1&ok==1){
  levels<-seq(0,1,length=nl)
  A<-subset(X,X$alpha==1)
  cut1<-min(as.numeric(row.names(A)))
  cut2<-max(as.numeric(row.names(A)))
  Left<-X[1:cut1,]
  Right<-X[cut2:nrow(X),]
  L1<-approx(Left$alpha,Left$x, method="linear", rule = 2,n=nl)
  L2<-cbind(L1$y,L1$x)
  L3<-L2
  L4<-data.frame(x=L3[,1],alpha=L3[,2])

  R1<-approx(Right$alpha,Right$x, method="linear", rule = 2,n=nl)
  R2<-cbind(R1$y,R1$x)
  R3<-R2[nrow(R2):1,]
  R4<-data.frame(x=R3[,1],alpha=R3[,2])
  E<-rbind(L4,R4)
  invisible(E)
 }
}

