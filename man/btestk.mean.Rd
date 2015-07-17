\name{btestk.mean}
\alias{btestk.mean}
\title{
Multi-sample bootstrap test for the equality of the mean of FRVs
}
\description{
Given a list \code{XXX} of length \code{k} sublists of polygonal fuzzy numbers the function first checks if each element of the sublists has the correct format and if the alpha-levels of all input fuzzy numbers coincide. The vector \code{sel} contains the numbers of the sublists the user wants to filter to. After filtering the relevant part of \code{XXX} the function computes the test-statistic, which compares the sum of the distances of the groups means and the overall mean with the sum of the group variances. Before doing the resampling \code{length(sel)} new samples are calculated by adding to each element of every fixed group the sum of all means of the other groups. Based on these \code{length(sel)} new samples \code{B} values of the (bootstrap) test statistic are calculate. The returned p-value is calculated as the portion of the obtained values of the bootstrap statistic that are greater than the value of the test-statistic. If \code{pic}=1 then the sample means of the via \code{sel} selected samples from \code{XXX} and the total mean are plotted in one window and the ecdf of the bootstrap statistic in another one, otherwise no plot is produced. For a more detailed explanation see the papers [1] and [2] below.
}
\usage{
btestk.mean(XXX, sel, theta = 1/3, B = 100, pic = 1)
}
\arguments{
  \item{XXX}{
...  A list of sublists, each of which contains polygonal fuzzy numbers
}
  \item{sel}{
...vector, selection of number of the samples (sublists) to be considered
}
  \item{theta}{
...numeric and >0
}
 \item{B}{
...integer, by default \code{B}=100.
}
  \item{pic}{
...numeric, if \code{pic}=1 then the sample means of the via \code{sel} selected samples from \code{XXX} and the total mean are plotted in one window and the ecdf of the bootstrap statistic in another one. By default \code{pic}=1.
}
}
\details{
See examples
}
\value{
Given input \code{XXX} in the correct format, the function returns the p-value of the two-sided test.
}
\references{
[1] Colubi, A.: \emph{Statistical inference about the means of fuzzy random variables: Applications to the analysis of fuzzy- and real-valued data}, Fuzzy Sets and Systems, 160(3), pp. 344-356 (2009)
     
[2] Gil, M.A.; Montenegro, M.; Gonzalez-Rodriguez, G.; Colubi, A.; Casals, R.: \emph{Bootstrap approach to the multi-sample test of means with imprecise data}, Computational Statistics and Data Analysis, 51(1), pp. 148-162  (2006)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
The function is quite slow.\cr
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{Mmean}}, \code{\link{Bvar}}, \code{\link{bertoluzza}},  \code{\link{btest.mean}}, \code{\link{btest2.mean}}
}
\examples{
#Example 1: very small B only for testing purpose
data(Trees)
sel<-c(1,2,3)
b<-btestk.mean(Trees,sel,B=5)
b

#Example 2: run for bigger B
#b<-btestk.mean(Trees,sel,100)
#b

## The function is currently defined as
function (XXX, sel, theta = 1/3, B = 100, pic = 1) 
{
    K <- length(XXX)
    ks <- length(sel)
    if (ks > K) {
        print("you can not select more variables than the ones contained in the sample XXX")
    }
    if (ks <= 1) {
        print("you have to select at least two variables (in XXX)")
    }
    if (ks <= K & ks > 1) {
        YYY <- vector("list", length = ks)
        nobs <- rep(0, ks)
        sel <- sort(sel)
        for (i in 1:ks) {
            YYY[[i]] <- XXX[[sel[i]]]
            nobs[i] <- length(YYY[[i]])
        }
        ZZ <- vector("list", length = sum(nobs))
        ZZ[1:nobs[1]] <- YYY[[1]][1:nobs[1]]
        selsum <- cumsum(nobs)
        for (i in 1:(ks - 1)) {
            ZZ[(selsum[i] + 1):(selsum[i + 1])] <- YYY[[i + 1]][1:nobs[i + 
                1]]
        }
        temp_sum <- Msum(ZZ)
        if (nrow(temp_sum) > 1) {
            nl <- nrow(temp_sum)/2
            sample_mean <- vector("list", length = ks)
            sample_sum <- vector("list", length = ks)
            total_mean <- vector("list", length = ks)
            sample_variance <- rep(0, ks)
            for (i in 1:ks) {
                sample_mean[[i]] <- Mmean(YYY[[i]], 0)
                sample_sum[[i]] <- sc_mult(sample_mean[[i]], 
                  nobs[i])
                sample_variance[i] <- Bvar(YYY[[i]], theta)
            }
            total_mean <- sc_mult(Msum(sample_sum), 1/sum(nobs))
            if (pic == 1) {
                lower <- sample_mean[[1]]$x[1]
                upper <- sample_mean[[1]]$x[2 * nl]
                for (i in 2:ks) {
                  lower <- min(lower, sample_mean[[i]]$x[1])
                  upper <- max(upper, sample_mean[[i]]$x[2 * 
                    nl])
                }
                legend_name <- paste(rep("group ", ks), sel, 
                  sep = "")
                limx <- c(lower, upper) + c(0, (upper - lower)/4)
                color <- colorRampPalette(c("green", "blue", 
                  "red"))(ks)
                plot(total_mean, type = "l", xlim = limx, lwd = 2, 
                  xlab = NA, ylab = expression(alpha), col = "black", 
                  main = paste("Total mean (black) and group means", 
                    "\n", "(group mean colour ranging from green to blue to red)", 
                    sep = ""), cex.main = 1)
                for (i in 1:ks) {
                  lines(sample_mean[[i]], type = "l", lwd = 1.5, 
                    col = color[i])
                }
                lines(total_mean, type = "l", col = "black", 
                  lwd = 2)
                if (ks <= 10) {
                  legend(upper, 1, legend_name, col = color, 
                    text.col = "black", lty = rep(1, ks), cex = 0.8)
                }
            }
            total_variance <- sum(sample_variance)
            temp <- rep(0, ks)
            for (i in 1:ks) {
                temp[i] <- bertoluzza(sample_mean[[i]], total_mean, 
                  theta)^2
            }
            test_statistic <- sum(nobs * temp)/total_variance
            samplestar <- list()
            for (i in 1:ks) {
                samplestar[[i]] <- list()
                relevant <- setdiff(seq(1, ks, by = 1), i)
                Mean_list <- vector("list", length = length(relevant))
                for (m in 1:length(relevant)) {
                  Mean_list[[m]] <- Mmean(YYY[[relevant[m]]])
                }
                suplement <- Msum(Mean_list)
                for (j in 1:nobs[i]) {
                  samplestar[[i]][[j]] <- Msum(list(YYY[[i]][[j]], 
                    suplement))
                }
            }
            boot_sample <- list()
            boot_sample_mean <- list()
            boot_sample_sum <- list()
            boot_sample_variance <- list()
            boot_total_mean <- list()
            boot_total_variance <- rep(0, B)
            boot_test_statistic <- rep(0, B)
            for (b in 1:B) {
                print(b)
                boot_sample[[b]] <- list()
                boot_sample_mean[[b]] <- list()
                boot_sample_sum[[b]] <- list()
                boot_sample_variance[[b]] <- rep(0, ks)
                for (i in 1:ks) {
                  boot_sample[[b]][[i]] <- vector("list", length = nobs[i])
                  boot_sample[[b]][[i]] <- sample(samplestar[[i]], 
                    nobs[i], replace = TRUE)
                  boot_sample_mean[[b]][[i]] <- Mmean(boot_sample[[b]][[i]])
                  boot_sample_sum[[b]][[i]] <- sc_mult(boot_sample_mean[[b]][[i]], 
                    nobs[i])
                  boot_sample_variance[[b]][[i]] <- Bvar(boot_sample[[b]][[i]], 
                    theta)
                }
                boot_total_mean[[b]] <- sc_mult(Msum(boot_sample_sum[[b]]), 
                  1/sum(nobs))
                boot_total_variance[[b]] <- sum(boot_sample_variance[[b]])
                temp <- rep(0, ks)
                for (m in 1:ks) {
                  temp[i] <- bertoluzza(boot_sample_mean[[b]][[i]], 
                    boot_total_mean[[b]], theta)^2
                }
                boot_test_statistic[b] <- sum(nobs * temp)/boot_total_variance[b]
            }
            if (pic == 1) {
                dev.new()
                limx <- c(min(c(boot_test_statistic, test_statistic)), 
                  max(c(boot_test_statistic, test_statistic)))
                plot(ecdf(boot_test_statistic), xlab = NA, ylab = NA, 
                  xlim = limx, do.points = FALSE, main = paste("Ecdf of T*"), 
                  cex.main = 1, lwd = 1.5)
                abline(a = NULL, b = NULL, v = test_statistic, 
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
\keyword{univar} % __ONLY ONE__ keyword per line
