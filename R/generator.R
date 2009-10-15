generator <-
function(V,pertV=list(dist="norm",par=c(0,1)),
   pertL=list(dist="chisq",par=c(1)),pertR=list(dist="chisq",par=c(1))){
 #V...expectation
 nl<-nrow(V)/2
 B<-decomposer(V)
 if(nrow(B)<=1){return(c(NA,NA))}
 
 vc<-B$coor[nl+1]
 cl<-B$coor[1:nl]
 cr<-B$coor[(nl+2):(2*nl+1)]

#filter to allowed cases:
 allowed<-c("unif","norm","chisq","lnorm")
 if(!pertV$dist%in%allowed[1:2]){
  print("chosen distributions for the perturbations of the centre of the 1-cut must be normal or uniform")
  return(c(NA,NA)) 
  }
  if(!pertL$dist%in%allowed[3:4]|!pertR$dist%in%allowed[3:4]){  
   print("chosen distributions for the (left/right) perturbations must be chisquareor lognormal")
   return(c(NA,NA)) 
  }   
  
#perturbation of VC:
 if(pertV$dist=="norm"){
  if(pertV$par[1]!=0){
   print("expectation of perturbation of the mid of the 1-cut must have expectation 0")
   return(c(NA,NA))
   }
  VC<-rnorm(1,0,pertV$par[2])+vc
  }
 if(pertV$dist=="unif"){
  if((pertV$par[1]+pertV$par[2])!=0){
   print("expectation of perturbation of the mid of the 1-cut must have expectation 0")
   return(c(NA,NA))
   }
   VC<-runif(1,pertV$par[1],pertV$par[2])+vc
  }
  
#perturbation of left part
  if(pertL$dist=="chisq"){
   if(pertL$par[1]!=1){
    print("expectation of (left) perturbation must have expectation 1 and be nonnegativ")
    return(c(NA,NA))
   }
   perl<-rchisq(nl, 1)
   }
  if(pertL$dist=="lnorm"){
   if(exp(pertL$par[1]+pertL$par[2]^2/2)!=1){
    print("expectation of (left) perturbation must have expectation 1 and be nonnegativ")
    return(c(NA,NA))
    }
   perl<-rlnorm(nl,pertL$par[1],pertL$par[2])
   }  
#perturbation of left part   
  if(pertR$dist=="chisq"){
   if(pertR$par[1]!=1){
    print("expectation of (right) perturbation must have expectation 1 and be nonnegativ")
    return(c(NA,NA))
    }
   perr<-rchisq(nl, 1)
   }
  if(pertR$dist=="lnorm"){
   if(exp(pertR$par[1]+pertR$par[2]^2/2)!=1){
    print("expectation of (left) perturbation must have expectation 1 and be nonnegativ")
    return(c(NA,NA))
    }
   perl<-rlnorm(nl,pertR$par[1],pertR$par[2])
   }   
   
  CL<-cl*perl
  CR<-cr*perr

  XL<-(VC-cumsum(CL[nl:1]))[nl:1]
  XR<-(VC+cumsum(CR))

 X<-data.frame(x=c(XL,XR),alpha=V$alpha)
 return(X)
 }

