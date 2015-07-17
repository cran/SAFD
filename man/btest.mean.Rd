\name{btest.mean}
\alias{btest.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
One-sample bootstrap test for the mean of a FRV
}
\description{
Given a sample \code{XX} of polygonal fuzzy numbers and a polygonal fuzzy number \code{V} the function first checks if each element of \code{XX} and \code{V} has the correct format and if the alpha-levels of all input fuzzy numbers coincide. In case yes, the function computes the standardized mean squared Bertoluzza-distance from the sample mean to \code{V} as test-statistic. Afterwards for \code{B} bootstrap samples the (bootstrap) statistic is calculated. The returned p-value is calculated as the portion of the obtained values of the bootstrap statistic that are greater than the value of the test-statistic. Furthermore, if \code{pic}=1 sample mean and \code{V} are plotted in one window and the ecdf of the bootstrap statistic in another one. For detailed explanation see papers [1] and [2] below.
}
\usage{
btest.mean(XX, V, theta = 1/3, B = 1000, pic = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{XX}{
...list of polygonal fuzzy numbers (the functions implicitly checks the conditions). 
}
  \item{V}{
...polygonal fuzzy number that is tested to be the mean of the FRV.
}
   \item{theta}{
...numeric and >0, see \code{bertoluzza}
}
  \item{B}{
...integer, by default \code{B}=1000.
}
  \item{pic}{
...numeric, if \code{pic}=1 sample mean and \code{V} are plotted in one window and the ecdf of the bootstrap statistic in another one. By default \code{pic}=1.
}
}
\details{
See examples
}
\value{
Given input \code{XX} and \code{V} in the correct format, the function returns the p-value of the two-sided bootstrap test that the expectation is \code{V}.
}
\references{
[1] Colubi, A.: \emph{Statistical inference about the means of fuzzy random variables: Applications to the analysis of fuzzy- and real-valued data}, Fuzzy Sets and Systems, 160(3), pp. 344-356 (2009)\cr
[2] Montenegro, M., Colubi, A., Casals, M.R., Gil, M.A.: \emph{Asymptotic and bootstrap techniques for testing the expected value of a fuzzy random variable}, Metrika, 59, pp. 31-49  (2004)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
The function is quite slow.\cr
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{Mmean}}, \code{\link{Bvar}}, \code{\link{bertoluzza}},  \code{\link{btest2.mean}}, \code{\link{btestk.mean}}
}
\examples{
#Example 1: run for bigger sample sizes:
data(XX)
V<-translator(XX[[3]],50)
V2<-V
SS<-vector("list",length=50)
for (j in 1:50){
 SS[[j]]<-generator(V2,)
 }
b<-btest.mean(SS,V2,B=10)
b

#Example 2: takes some time to run:
#data(Trees)
#V<-Trees[[1]][[47]]
#b<-btest.mean(Trees[[1]],V,100)
#b


## The function is currently defined as
function (XX, V, theta = 1/3, B = 1000, pic = 1) 
{
    k <- length(XX)
    YY <- vector("list", length = (k + 1))
    YY[1:k] <- XX[1:k]
    YY[[k + 1]] <- V
    temp_sum <- Msum(YY)
    if (nrow(temp_sum) > 1) {
        nl <- nrow(V)/2
        nobs <- k
        sample_mean <- Mmean(XX, 0)
        sample_variance <- Bvar(XX, theta) * (nobs/(nobs - 1))
        test_statistic <- bertoluzza(sample_mean, V, theta)^2/sample_variance
        if (pic == 1) {
            lower <- min(sample_mean$x[1], V$x[1])
            upper <- max(sample_mean$x[2 * nl], V$x[2 * nl])
            limx <- c(min(lower) - 0.25, max(upper) + 0.25)
            plot(sample_mean, type = "l", xlim = limx, lwd = 2, 
                xlab = NA, ylab = expression(alpha), cex.main = 1, 
                lty = "dashed", main = paste("Sample mean (dashed line) and V (solid line)", 
                  sep = ""))
            lines(V, type = "l", lwd = 2, lty = "solid")
        }
        boot_sample <- replicate(B, sample(XX, nobs, replace = TRUE))
        boot_sample_mean <- apply(boot_sample, 2, Mmean)
        boot_sample_variance <- apply(boot_sample, 2, Bvar, theta) * 
            (nobs/(nobs - 1))
        boot_test_statistic <- rep(0, B)
        for (i in 1:B) {
            boot_test_statistic[i] <- bertoluzza(boot_sample_mean[[i]], 
                sample_mean, theta)^2/boot_sample_variance[i]
        }
        if (pic == 1) {
            dev.new()
            limx <- c(min(c(boot_test_statistic, test_statistic)), 
                max(c(boot_test_statistic, test_statistic)))
            plot(ecdf(boot_test_statistic), xlab = NA, ylab = NA, 
                xlim = limx, do.points = FALSE, main = paste("Ecdf of T*"), 
                cex.main = 1, lwd = 1.5)
            abline(a = NULL, b = NULL, v = test_statistic, col = "black", 
                lty = "dotted", lwd = 3)
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
