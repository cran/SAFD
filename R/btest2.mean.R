btest2.mean <-
function(XX,YY,B=1000,pic=1){
 #XX, YY ... independent samples
 #B...number of bootstrap replicates
 kx<-length(XX)
 ky<-length(YY)
 #bind XX and YY in list to simplify check for compatibility
 ZZ<-vector("list",length=(kx+ky))
 ZZ[1:kx]<-XX[1:kx]
 ZZ[(kx+1):(kx+ky)]<-YY[1:ky]
 
 temp_mean<-Mmean(ZZ)
 if(nrow(temp_mean)>1){
  n1obs<-kx
  n2obs<-ky
 
  nl<-nrow(XX[[1]])/2
  #compute the test statistic
  sample_mean_XX <-Mmean(XX,0)
  sample_mean_YY <-Mmean(YY,0)
  sample_variance <- Bvar(XX, theta=1/3)/(n1obs-1) + Bvar(YY, theta=1/3)/(n2obs-1)
  test_statistic <-bertoluzza(sample_mean_XX,sample_mean_YY,theta=1/3)^2/sample_variance

  if(pic==1){
   lower<-min(sample_mean_XX$x[1],sample_mean_YY$x[1])
   upper<-max(sample_mean_XX$x[2*nl],sample_mean_YY$x[2*nl])
   limx<-c(min(lower),max(upper))
   plot(sample_mean_XX,type="l", xlim=limx,lwd=2,xlab="x", ylab="alpha",cex.main=1, col="black",
          main=paste("Sample mean first sample and second sample (in red)",sep=""))
   lines(sample_mean_YY,type="l", lwd=2,col="red")
   }
  XXstar<-vector("list",length=n1obs)
   for (i in 1:n1obs){
     XXstar[[i]]<-Msum(list(XX[[i]],sample_mean_YY))
   }
  YYstar<-vector("list",length=n2obs)
   for (i in 1:n2obs){
     YYstar[[i]]<-Msum(list(YY[[i]],sample_mean_XX))
   }

  #####bootstrap technique to test the mean equality

  #sample bootstrap of XX
  boot_sample_XX<-replicate(B,sample(XXstar, n1obs,replace=TRUE))
  #list with the means of the sample bootstrap of XX
  boot_sample_mean_XX <- apply(boot_sample_XX,2,Mmean)
  #vector with the sample variances of the sample bootstrap of XX
  boot_sample_variance_XX <- apply(boot_sample_XX,2,Bvar,1/3)

  #sample bootstrap of YY
  boot_sample_YY<-replicate(B,sample(YYstar, n2obs,replace=TRUE))
  #list with the means of the sample bootstrap of YY
  boot_sample_mean_YY <- apply(boot_sample_YY,2,Mmean)
  #vector with the sample variances of the sample bootstrap of YY
  boot_sample_variance_YY <- apply(boot_sample_YY,2,Bvar,1/3)

  boot_test_statistic<-rep(0,B)
   for (i in 1:B){
    boot_test_statistic[i] <-bertoluzza(boot_sample_mean_XX[[i]],boot_sample_mean_YY[[i]],1/3)^2/(boot_sample_variance_XX[i]/(n1obs-1)+boot_sample_variance_YY[i]/(n2obs-1))
    #print(boot_test_statistic[i])
   }
  if(pic==1){
   dev.new()
   limx<-c(min(c(boot_test_statistic,test_statistic)),max(c(boot_test_statistic,test_statistic)))
   #print(test_statistic)
   plot(ecdf(boot_test_statistic),xlab=NA,ylab=NA,xlim=limx, do.points = FALSE, main=paste("Ecdf of T*"),cex.main=1,lwd=1.5)
    #cex.axis=1.3,cex.lab=1.3)
   abline(a = NULL, b = NULL, v = test_statistic,col="red")
   TS<-test_statistic
   mtext(paste("T=",round(TS,2),sep=""), at = TS,  side = 1, line = 2, col = "red", bg="white",cex=1.3)
   }
  pvalue<-mean(test_statistic<boot_test_statistic)
 invisible(pvalue)
 }
}

