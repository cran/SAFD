Bcor <-
function(XX,YY,theta=1/3){
  cova<-Bcov(XX,YY,theta)
  res<-cova/(Bvar(XX,theta)*Bvar(YY,theta))^0.5
  return(res)
  }

