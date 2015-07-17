\name{lrmodel}
\alias{lrmodel}
\title{
Linear Regression model 
}
\description{
The linear regression model Y=aX+eps for trapezoidal fuzzy numbers as described in [1] has been implemented, whereby the least-squares-minimization (with constraints) is done with respect to the Bertoluzza metric (with \code{theta}=1/3). Given lists \code{XX}, \code{YY} of polygonal fuzzy numbers the functions first checks (1) if each element of the two list is in the correct form (tested by \code{checking}), (2) if the alpha-levels of all elements in the two lists coincide and (3) if the lists have the same length. If all conditions are fulfilled the function automatically converts the fuzzy numbers in \code{XX} and \code{YY} in trapezoidal ones and returns the estimations for the parameters \code{a} and \code{B} - in case of double solutions both solutions are returned.
}
\usage{
lrmodel (XX, YY, theta = 1/3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers (the functions implicitly checks the conditions) having the same length as \code{YY} 
}
  \item{YY}{
...list of polygonal fuzzy numbers (the functions implicitly checks the conditions) having the same length as \code{XX} 
}
  \item{theta}{
...numeric and >0, see \code{bertoluzza}
}
}
\details{
See examples}
\value{
Given input \code{XX}, \code{YY} in the correct format the function returns a list containing the estimates for \code{a} and \code{B} (in case of double solutions both solutions are returned).
}
\references{
[1] Gonzalez-Rodriguez, G.; Blanco, A.; Colubi, A.; Lubiano, M.A.: \emph{Estimation of a simple linear regression model for fuzzy random variables}, Fuzzy Sets and Systems, 160(3), pp. 357-370 (2009) \cr
[2] Gil, M.A., Lopez, M.T., Lubiano, M.A., Montenegro, M.: \emph{Regression and correlation analyses of a linear relation between random intervals}, Test, 10(1), pp. 183-201 (2001)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{translator}}, \code{\link{Mmean}}, \code{\link{hukuhara}}, \code{\link{Bvar}}, \code{\link{Bcov}}, \code{\link{bertoluzza}}
}
\examples{
#Example 1 (crisp case):
XX<-vector("list",length=2)
XX[[1]]<-data.frame(x=c(1,1,1,1),alpha=c(0,1,1,0))
XX[[2]]<-data.frame(x=c(2,2,2,2),alpha=c(0,1,1,0))
YY<-list(length=2)
YY[[1]]<-data.frame(x=c(1,1,1,1),alpha=c(0,1,1,0))
YY[[2]]<-data.frame(x=c(2,2,2,2),alpha=c(0,1,1,0))
m<-lrmodel(XX,YY)
m

#Example 2:
data(XX)
V<-translator(XX[[3]],100)
XX<-vector("list",length=50)
YY<-XX
  for(i in 1:50){
   XX[[i]]<-generator(V,,,)
   YY[[i]]<-XX[[i]]
   YY[[i]]$x<-5*YY[[i]]$x+1
   }
m<-lrmodel(XX,YY)
m

#Example 3:
data(quality)
model1<-lrmodel(quality$land,quality$trees)
model1
model2<-lrmodel(quality$trees,quality$land)
model2


## The function is currently defined as
function (XX, YY, theta = 1/3) 
{
    kx <- length(XX)
    ky <- length(YY)
    if (kx != ky) {
        print("lists must have same length (i.e. input and output must have same sample size")
    }
    if (kx == ky) {
        nobs <- kx
        for (i in 1:nobs) {
            XX[[i]] <- translator(XX[[i]], 2)
            YY[[i]] <- translator(YY[[i]], 2)
        }
        ZZ <- vector("list", length = (2 * nobs))
        ZZ[1:nobs] <- XX[1:nobs]
        ZZ[(nobs + 1):(2 * nobs)] <- YY[1:nobs]
        temp_sum <- Msum(ZZ)
        if (nrow(temp_sum) > 1) {
            XXs <- rep(0, nobs)
            YYs <- rep(0, nobs)
            XXl <- rep(0, nobs)
            XXr <- rep(0, nobs)
            YYl <- rep(0, nobs)
            YYr <- rep(0, nobs)
            M <- data.frame(XXs = XXs, YYs = YYs, XXl = XXl, 
                YYl = YYl, XXr = XXr, YYr = YYr)
            for (i in 1:nobs) {
                M$XXs[i] <- 0.5 * (XX[[i]]$x[3] - XX[[i]]$x[2])
                M$YYs[i] <- 0.5 * (YY[[i]]$x[3] - YY[[i]]$x[2])
                M$XXl[i] <- (XX[[i]]$x[2] - XX[[i]]$x[1])
                M$YYl[i] <- (YY[[i]]$x[2] - YY[[i]]$x[1])
                M$XXr[i] <- (XX[[i]]$x[4] - XX[[i]]$x[3])
                M$YYr[i] <- (YY[[i]]$x[4] - YY[[i]]$x[3])
            }
            M1 <- subset(M, M$XXs > 0 & M$XXl > 0 & M$XXr > 0)
            if (nrow(M1) == 0) {
                Feas <- c(NA, NA)
            }
            if (nrow(M1) > 0) {
                M1$valneg1 <- M1$YYs/M1$XXs
                M1$valpos2 <- M1$YYl/M1$XXl
                M1$valneg2 <- M1$YYr/M1$XXl
                M1$valpos3 <- M1$YYr/M1$XXr
                M1$valneg3 <- M1$YYl/M1$XXr
                bmax0 <- min(M1$valneg1, M1$valpos2, M1$valpos3)
                amax0 <- min(M1$valneg1, M1$valneg2, M1$valneg3)
                Feas <- c(-amax0, bmax0)
            }
            varX <- Bvar(XX, theta)
            covXY <- Bcov(XX, YY, theta)
            mXX <- list()
            for (i in 1:nobs) {
                mXX[[i]] <- sc_mult(XX[[i]], -1)
            }
            covmXY <- Bcov(mXX, YY, theta)
            beta <- 0
            if (covmXY > 0) {
                if (is.na(Feas[1]) == TRUE) {
                  beta <- 1
                }
                if (is.na(Feas[1]) == FALSE) {
                  beta <- min(1, Feas[1] * varX/covmXY)
                }
            }
            gamma <- 0
            if (covXY > 0) {
                if (is.na(Feas[1]) == TRUE) {
                  gamma <- 1
                }
                if (is.na(Feas[1]) == FALSE) {
                  gamma <- min(1, Feas[2] * varX/covXY)
                }
            }
            if (gamma == 0 | beta == 0) {
                a <- gamma * covXY/varX - beta * covmXY/varX
                Best <- hukuhara(sc_mult(Mmean(XX), a), Mmean(YY), 
                  0)
                Result <- list(a = a, B = Best)
            }
            if (gamma != 0 & beta != 0) {
                cond <- covmXY/covXY - (2 * gamma - gamma^2)/(2 * 
                  beta - beta^2)
                if (cond > 0) {
                  a <- beta * covmXY/varX
                  Best <- hukuhara(sc_mult(Mmean(XX), a), Mmean(YY), 
                    0)
                  Result <- list(a = a, B = Best)
                }
                if (cond < 0) {
                  a <- gamma * covXY/varX
                  Best <- hukuhara(sc_mult(Mmean(XX), a), Mmean(YY), 
                    0)
                  Result <- list(a = a, B = Best)
                }
                if (cond == 0) {
                  a <- c(beta * covmXY/varX, gamma * covXY/varX)
                  B <- vector("list", length = 2)
                  B[[1]] <- hukuhara(sc_mult(Mmean(XX), a[1]), 
                    Mmean(YY), 0)
                  B[[2]] <- hukuhara(sc_mult(Mmean(XX), a[2]), 
                    Mmean(YY), 0)
                  Result <- list(a = a, B = B)
                }
            }
            invisible(Result)
        }
    }
  }
}
\keyword{ regression }
\keyword{ models }
