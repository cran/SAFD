\name{btest2.mean}
\alias{btest2.mean}
\title{
Two-sample bootstrap test on the equality of mean of two FRVs
}
\description{
Given two samples \code{XX} and \code{YY} of polygonal fuzzy numbers the function first checks if each element of \code{XX} and \code{YY} has the correct format and if the alpha-levels of all input fuzzy numbers coincide. In case yes, the function compute the test statistic described in [1] below. Before doing the resampling \code{Mmean(YY)} is added to each element of \code{XX} and vice versa. Based on these two new samples \code{B} values of the test statistic are calculate. The returned p-value is calculated as the portion of the obtained values of the bootstrap statistic that are greater than the value of the test-statistic. If \code{pic}=1 then the sample means of \code{XX} and \code{YY} are plotted in one window and the ecdf of the bootstrap statistic in another one, otherwise no plot is produced. For detailed explanation see the papers [1] and [2] below.
}
\usage{
btest2.mean(XX, YY, theta = 1/3, B = 1000, pic = 1)
}
\arguments{
  \item{XX}{
...should be a list of polygonal fuzzy numbers (the functions implicitly checks the conditions) 
}
  \item{YY}{
...should be a list of polygonal fuzzy numbers (the functions implicitly checks the conditions) 
}
  \item{theta}{
...numeric and >0
}
  \item{B}{
...integer, by default \code{B}=1000.
}
  \item{pic}{
...numeric, if \code{pic}=1 then the sample means of \code{XX} and \code{YY} are plotted in one window and the ecdf of the bootstrap statistic in another one. By default \code{pic}=1.
}
}
\details{
See examples
}
\value{
Given input \code{XX} and \code{YY} in the correct format, the function returns the p-value of the two-sided bootstrap test.
}
\references{
[1] Colubi, A.: \emph{Statistical inference about the means of fuzzy random variables: Applications to the analysis of fuzzy- and real-valued data}, Fuzzy Sets and Systems, 160(3), pp. 344-356 (2009) 
      
[2] Montenegro, M., Casals, M.R., Lubiano, M.A., Gil, M.A.: \emph{Two-sample hypothesis tests of means of a fuzzy random variable}, Information Sciences, Vol. 133(1-2), pp. 89-100 (2001)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
The function is quite slow.\cr
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{Mmean}}, \code{\link{Bvar}}, \code{\link{bertoluzza}}, \code{\link{btest.mean}}, \code{\link{btestk.mean}}
}
\examples{
#Example 1: run for bigger B
data(XX)
X<-translator(XX[[1]],20)
Y<-translator(XX[[2]],20)
XX<-vector("list",length=30)
for (j in 1:30){
 XX[[j]]<-generator(X,)
 }
YY<-vector("list",length=20)
for (j in 1:20){
 YY[[j]]<-generator(Y,)
 }
b<-btest2.mean(XX,YY,B=10)
b

#Example 2: takes some time in the current version:
#data(Trees)
#b<-btest2.mean(Trees[[1]],Trees[[2]],50)
#b
#b<-btest2.mean(Trees[[1]],Trees[[3]],50)
#b

## The function is currently defined as
function (XX, YY, theta = 1/3, B = 1000, pic = 1) 
{
    kx <- length(XX)
    ky <- length(YY)
    ZZ <- vector("list", length = (kx + ky))
    ZZ[1:kx] <- XX[1:kx]
    ZZ[(kx + 1):(kx + ky)] <- YY[1:ky]
    temp_sum <- Msum(ZZ)
    if (nrow(temp_sum) > 1) {
        n1obs <- kx
        n2obs <- ky
        nl <- nrow(XX[[1]])/2
        sample_mean_XX <- Mmean(XX, 0)
        sample_mean_YY <- Mmean(YY, 0)
        sample_variance <- Bvar(XX, theta)/(n1obs - 1) + Bvar(YY, 
            theta)/(n2obs - 1)
        test_statistic <- bertoluzza(sample_mean_XX, sample_mean_YY, 
            theta)^2/sample_variance
        if (pic == 1) {
            lower <- min(sample_mean_XX$x[1], sample_mean_YY$x[1])
            upper <- max(sample_mean_XX$x[2 * nl], sample_mean_YY$x[2 * 
                nl])
            limx <- c(min(lower) - 0.25, max(upper) + 0.25)
            plot(sample_mean_XX, type = "l", xlim = limx, lwd = 2, 
              xlab = NA, ylab = expression(alpha), cex.main = 1, lty = "solid", 
              main = paste("Sample mean 1st sample(solid) and 2nd sample(dashed)", 
                  sep = ""))
            lines(sample_mean_YY, type = "l", lwd = 2, lty = "dashed")
        }
        XXstar <- vector("list", length = n1obs)
        for (i in 1:n1obs) {
            XXstar[[i]] <- Msum(list(XX[[i]], sample_mean_YY))
        }
        YYstar <- vector("list", length = n2obs)
        for (i in 1:n2obs) {
            YYstar[[i]] <- Msum(list(YY[[i]], sample_mean_XX))
        }
        boot_sample_XX <- replicate(B, sample(XXstar, n1obs, 
            replace = TRUE))
        boot_sample_mean_XX <- apply(boot_sample_XX, 2, Mmean)
        boot_sample_variance_XX <- apply(boot_sample_XX, 2, Bvar, 
            theta)
        boot_sample_YY <- replicate(B, sample(YYstar, n2obs, 
            replace = TRUE))
        boot_sample_mean_YY <- apply(boot_sample_YY, 2, Mmean)
        boot_sample_variance_YY <- apply(boot_sample_YY, 2, Bvar, 
            theta)
        boot_test_statistic <- rep(0, B)
        for (i in 1:B) {
            boot_test_statistic[i] <- bertoluzza(boot_sample_mean_XX[[i]], 
                boot_sample_mean_YY[[i]], theta)^2/(boot_sample_variance_XX[i]/(n1obs - 
                1) + boot_sample_variance_YY[i]/(n2obs - 1))
        }
        if (pic == 1) {
            dev.new()
            limx <- c(min(c(boot_test_statistic, test_statistic)), 
                max(c(boot_test_statistic, test_statistic)))
            plot(ecdf(boot_test_statistic), xlab = NA, ylab = NA, 
                xlim = limx, do.points = FALSE, main = paste("Ecdf of T*"), 
                cex.main = 1, lwd = 1.5)
            abline(a = NULL, b = NULL, v = test_statistic, lty = "dotted", 
                lwd = 3)
            TS <- test_statistic
            mtext(paste("T=", round(TS, 2), sep = ""), at = TS, 
                side = 1, line = 2, col = "black", bg = "white", 
                cex = 1.3)
        }
        pvalue <- mean(test_statistic < boot_test_statistic)
        invisible(pvalue)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ univar }% __ONLY ONE__ keyword per line
