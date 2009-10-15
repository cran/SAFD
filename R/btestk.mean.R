btestk.mean <-
function(XXX, sel, B=50,pic=1){
 #XXX ...list of independent samples
 #sel ...selection of the variables to be considered
 #B...number of bootstrap replicates


 K<-length(XXX)
 ks<-length(sel)

 #checking
  if(ks>K){
   print("you can not select more variables than the ones contained in the sample XXX")
   return(c(NA))
   }
  if(ks<=1){
   print("you have to select at least two variables (in XXX)")
   return(c(NA))
   }


 #checking samples in XXX are compatible
  YYY<-list(length=ks)
  nobs<-rep(0,ks)
  sel<-sort(sel)
  for(i in 1:ks){
    YYY[[i]]<-XXX[[sel[i]]]
    nobs[i]<-length(YYY[[i]])
   }
  ZZ<-list(length=sum(nobs))
  ZZ[1:nobs[1]]<-YYY[[1]][1:nobs[1]]
  selsum<-cumsum(nobs)
  for(i in 1:(ks-1)){
    ZZ[(selsum[i]+1):(selsum[i+1])] <-YYY[[i+1]][1:nobs[i+1]]
  }
 #checking done  
   temp_mean<-Mmean(ZZ)
   if(nrow(temp_mean)==1){
    return(c(NA))
   }

  nl<-nrow(temp_mean)/2
 #compute the test statistic
 sample_mean<-list(length=ks)
 sample_sum<-list(length=ks)
 total_mean <-list(length=ks)
 sample_variance<-rep(0,ks)

 for (i in 1:ks){
 	sample_mean[[i]]<-Mmean(YYY[[i]],0)
	sample_sum[[i]]<-sc_mult(sample_mean[[i]],nobs[i])
	sample_variance[i]<-Bvar(YYY[[i]],theta=1/3)
 }

 total_mean <- sc_mult(Msum(sample_sum),1/sum(nobs))
 
 #optional plotting of the group means
 #in case of at most 10 groups a legend is plotted
 if(pic==1){
   lower<-sample_mean[[1]]$x[1]
   upper<-sample_mean[[1]]$x[2*nl]
   for (i in 2:ks){
      lower<-min(lower,sample_mean[[i]]$x[1])
      upper<-max(upper,sample_mean[[i]]$x[2*nl])
   }
  legend_name<-paste(rep("group ",ks),sel,sep="") 
  limx<-c(lower,upper)+c(0,(upper-lower)/4)
  color<- colorRampPalette( c("green","blue","red"))(ks)
  plot(total_mean,type="l", xlim=limx,lwd=2,xlab="x", ylab="alpha",cex.main=1, col="black",
          main=paste("Total mean (black) and group means","\n", 
          "(group mean colour ranging from green to blue to red)",sep=""),
          cex.main=1)
  for (i in 1:ks){
  lines(sample_mean[[i]],type="l", lwd=1.5,col=color[i])
  } 
  lines(total_mean,type="l",col="black",lwd=2)
  if(ks<=10){
   legend(upper, 1, legend_name, col = color, text.col = "black", lty = rep(1,ks),cex=0.8)
  }
 }

 total_variance <- sum(sample_variance)

 temp<-rep(0,ks)
 for (i in 1:ks){
  temp[i]<-bertoluzza(sample_mean[[i]],total_mean,theta=1/3)^2
 }
 test_statistic <-sum(nobs*temp)/total_variance


 #sample under H0
 samplestar<-list()
  for (i in 1:ks){
  samplestar[[i]]<-list()
  #calculate Mmeans of all groups different to the i-th and their sum
  relevant<-setdiff(seq(1,ks,by=1),i)
  Mean_list<-list(length=length(relevant))
  for (m in 1:length(relevant)){
    Mean_list[[m]]<-Mmean(YYY[[relevant[m]]])
  }
  suplement<-Msum(Mean_list)
  #add suplement to all observations of variable i
  for (j in 1:nobs[i]){
   samplestar[[i]][[j]] <- Msum(list(YYY[[i]][[j]],suplement))
  }
 }

 ########bootstrap technique


 boot_sample<-list()
 boot_sample_mean<-list()
 boot_sample_sum<-list()
 boot_sample_variance<-list()
 boot_total_mean<-list()
 boot_total_variance<-rep(0,B)
 boot_test_statistic<-rep(0,B)

	for (b in 1:B){
	 #print(b)
	 boot_sample[[b]]<-list()
	 boot_sample_mean[[b]]<-list()
	 boot_sample_sum[[b]]<-list()
	 boot_sample_variance[[b]]<-rep(0,ks)

			for (i in 1:ks){
			boot_sample[[b]][[i]]<-list(length=nobs[i])
			boot_sample[[b]][[i]] <- sample(samplestar[[i]], nobs[i],replace=TRUE) #sample bootstrap of Xi
			boot_sample_mean[[b]][[i]] <- Mmean(boot_sample[[b]][[i]])	
			boot_sample_sum[[b]][[i]] <- sc_mult(boot_sample_mean[[b]][[i]],nobs[i])
			boot_sample_variance[[b]][[i]] <- Bvar(boot_sample[[b]][[i]],1/3)	
			}
  
   boot_total_mean[[b]] <- sc_mult(Msum(boot_sample_sum[[b]]),1/sum(nobs))
	 boot_total_variance[[b]] <- sum(boot_sample_variance[[b]])
	 temp<-rep(0,ks)
	  for (m in 1:ks){
	   temp[i]<-bertoluzza(boot_sample_mean[[b]][[i]],boot_total_mean[[b]],theta=1/3)^2
    }
	 boot_test_statistic[[b]] <- sum(nobs*temp)/boot_total_variance[[b]]
  }

 pvalue<-mean(test_statistic<boot_test_statistic)
 return(pvalue)
}
