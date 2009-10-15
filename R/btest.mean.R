btest.mean <-
function(XX,V,B=1000){
 #XX...sample (list as always)
 #V...expectation we want to test for
 #B...number of bootstrap replicates
 #alpha...confidence level
 k<-length(XX)
 #bind XX and V in list to simplify check for compatibility
 YY<-list(length=(k+1))
 YY[1:k]<-XX[1:k]
 YY[[k+1]]<-V
 
 #it seems easier to simply use the implicit error checking in Msum (Mmean) 
 #to check if everything is in YY  
 temp_mean<-Mmean(YY)
 if(nrow(temp_mean)==1){
   return(c(NA))
  }
 
 nl<-nrow(V)/2
 
 nobs<-k
 sample_mean <-Mmean(XX,0)
 sample_variance <- Bvar(XX, theta=1/3)*(nobs/(nobs-1))
 test_statistic <-bertoluzza(sample_mean,V,theta=1/3)^2/sample_variance

 lower<-min(sample_mean$x[1],V$x[1])
 upper<-max(sample_mean$x[2*nl],V$x[2*nl])
 limx<-c(min(lower),max(upper))
 plot(sample_mean,type="l", xlim=limx,lwd=2,xlab="x", ylab="alpha",cex.main=1, col="black",
          main=paste("Sample mean and V (in red)",sep=""))
 lines(V,type="l", lwd=2,col="red") 
 
 boot_sample<-replicate(B,sample(XX, nobs,replace=TRUE))
 boot_sample_mean <- apply(boot_sample,2,Mmean)   #list with the means of the sample bootstrap
 boot_sample_variance <- apply(boot_sample,2,Bvar,1/3)*(nobs/(nobs-1)) #vector with the sample variances of the sample bootstrap
 
 boot_test_statistic<-rep(0,B)
  for (i in 1:B){
   boot_test_statistic[i] <-bertoluzza(boot_sample_mean[[i]],sample_mean,1/3)^2/boot_sample_variance[i]
   #print(boot_test_statistic[i])
  }
 pvalue<-mean(test_statistic<boot_test_statistic)
 return(pvalue)
}

