Fquantile <-
function(XX,p=c(0,0.25,0.5,0.75,1)){
  #calculates the quantile of k polygonial fuzzy numbers with same levels
  #if necessary just use translator first to assure same alpha levels
  #use Msum to check if input ok
  qnames<-names(quantile(seq(0,1,length=101),probs=p))
  m<-Mmean(XX)
  nl<-nrow(m)
  ss<-length(XX)
  if(is.null(m)==0){
   X<-matrix(0,nrow=nl,ncol=ss)
   for(i in 1:ss){
    X[,i]<-XX[[i]]$x
   }
   Quants<-matrix(0,nrow=nl,ncol=length(p))
   for(j in 1:nl){
     Quants[j,1:length(p)]<-as.numeric(quantile(X[j,1:ss],probs=p))
   }
   #return list containing the quantiles
   QQ<-vector("list",length=length(p))
   names(QQ)<-qnames
   for(i in 1:length(p)){
    QQ[[i]]<-data.frame(x=Quants[,i],alpha=m$alpha)
   }
   invisible(QQ)
  }
}
