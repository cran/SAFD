Bcor <-
function(XX,YY,theta=1/3){
  cova<-Bcov(XX,YY,theta)
  if(is.null(cova)==0){
   res<-cova/(Bvar(XX,theta)*Bvar(YY,theta))^0.5
   invisible(res)
   }
  }

