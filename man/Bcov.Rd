\name{Bcov}
\alias{Bcov}
\title{
(Sample) Covariance
}
\description{
The sample covariance of two samples of polygonal fuzzy numbers with respect to the Bertoluzza distance is calculated. Given lists \code{XX}, \code{YY} of polygonal fuzzy numbers the functions first checks (1) if each element of the two lists is in the correct form (tested by \code{checking}), (2) if the alpha-levels of all elements in the two lists coincide and (3) if the lists have the same length. If all conditions are fulfilled the Bertoluzza covariance will be returned. If not the \code{translator} function can be used to transform the elements of the lists in the correct format. For details see [1] from below, and replace the kernel K with the expression induced by the Bertoluzza metric. The parameter \code{theta} has to fulfill \code{theta}>0.
}
\usage{
Bcov(XX, YY, theta = 1/3)
}
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
See examples
}
\value{
Given input \code{XX}, \code{YY} in the correct format the function returns the Bertoluzza covariance of the samples \code{XX}, \code{YY}.
}
\references{
[1] Gonzalez-Rodriguez, G.; Blanco, A.; Colubi, A.; Lubiano, M.A.: \emph{Estimation of a simple linear regression model for fuzzy random variables}, Fuzzy Sets and Systems, 160(3), pp. 357-370 (2009)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{Mmean}},  \code{\link{Bvar}}, \code{\link{lrmodel}}
}
\examples{
#Example 1:
data(XX)
V<-translator(XX[[3]],100)
XX<-vector("list",length=10)
YY<-XX
  for(i in 1:10){
   XX[[i]]<-generator(V,,,)
   YY[[i]]<-XX[[i]]
   YY[[i]]$x<-5*YY[[i]]$x+1
   }
b<-Bcov(XX,YY,1/3)
b

#Example 2:
data(XX)
V<-translator(XX[[3]],51)
XX<-vector("list",length=500)
YY<-XX
  for(i in 1:500){
   XX[[i]]<-generator(V,,,)
   YY[[i]]<-XX[[i]]
   YY[[i]]$x<-5*YY[[i]]$x+1
   }
b<-Bcov(XX,YY,1/3)
b


#Example 3:
data(quality)
Quality_Cov<-Bcov(quality$trees,quality$land,1/3)
Quality_Cov


## The function is currently defined as
function (XX, YY, theta = 1/3) 
{
    kx <- length(XX)
    ky <- length(YY)
    if (kx != ky) {
        print("lists have to have same length (i.e. same sample sizes)")
    }
    int_product <- function(x, y) {
        if (length(x) != length(y)) {
            return(print("input vectors must have same length"))
        }
        if (length(x) == length(y)) {
            k <- length(x) - 1
            delta <- 1/k
            pr <- x * y
            middle <- (x[1:k] + x[2:(k + 1)]) * (y[1:k] + y[2:(k + 
                1)])
            values <- pr[1:k] + pr[2:(k + 1)] + middle
            integral <- sum(values) * delta/6
            invisible(integral)
        }
    }
    if (kx == ky) {
        ZZ <- vector("list", length = (2 * kx))
        ZZ[1:kx] <- XX[1:kx]
        ZZ[(kx + 1):(2 * kx)] <- YY[1:kx]
        temp_sum <- Msum(ZZ)
        if (nrow(temp_sum) > 1) {
            k <- length(XX)
            EX <- Mmean(XX)
            EY <- Mmean(YY)
            nl <- nrow(XX[[1]])/2
            midEX <- 0.5 * (EX$x[1:nl] + EX$x[(2 * nl):(nl + 
                1)])
            sprEX <- 0.5 * (EX$x[1:nl] - EX$x[(2 * nl):(nl + 
                1)])
            midEY <- 0.5 * (EY$x[1:nl] + EY$x[(2 * nl):(nl + 
                1)])
            sprEY <- 0.5 * (EY$x[1:nl] - EY$x[(2 * nl):(nl + 
                1)])
            contr_Emids <- int_product(midEX, midEY)
            contr_Espreads <- int_product(sprEX, sprEY)
            contr_mids <- rep(0, k)
            contr_spreads <- rep(0, k)
            for (i in 1:k) {
                z <- XX[[i]]$x
                w <- YY[[i]]$x
                midX <- 0.5 * (z[1:nl] + z[(2 * nl):(nl + 1)])
                sprX <- 0.5 * (z[1:nl] - z[(2 * nl):(nl + 1)])
                midY <- 0.5 * (w[1:nl] + w[(2 * nl):(nl + 1)])
                sprY <- 0.5 * (w[1:nl] - w[(2 * nl):(nl + 1)])
                contr_mids[i] <- int_product(midX, midY)/k
                contr_spreads[i] <- int_product(sprX, sprY)/k
            }
            cova <- (sum(contr_mids) - contr_Emids + theta * 
                (sum(contr_spreads) - contr_Espreads))
            invisible(cova)
        }
    }
  }
}

\keyword{ regression }
\keyword{ manip }
\keyword{ univar }

