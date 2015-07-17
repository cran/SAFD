\name{Msum}
\alias{Msum}
\title{
Minkowski sum
}
\description{
Given a list \code{XX} of polygonal fuzzy numbers the function first checks (1) if each element of the list is in the correct form (tested by \code{checking}) and (2) if the alpha-levels of all elements in the list coincide. If these two conditions are fulfilled the levelwise Minkowski-sum of all elements in the sample \code{XX} will be returned. If not the \code{translator} function can be used to transform the elements of the list in the correct format. 
}
\usage{
Msum(XX, pic = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers (the function implicitly checks the conditions)
}
  \item{pic}{
...numeric, if \code{pic}=1 then the sample mean of \code{XX} is printed. By default \code{pic}=0.
}
}
\details{
See examples
}
\value{
Given input \code{XX} in the correct format the function returns the Minkowski sum of the polygonal fuzzy numbers contained in the list.
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also \code{\link{checking}}, \code{\link{translator}}, \code{\link{Mmean}} 
}
\examples{
#Example 1:
X<-data.frame(x=c(0,1,1.5,3),alpha=c(0,1,1,0))
Y<-data.frame(x=c(1.25,2.75,2.75,5),alpha=c(0,1,1,0))
sum<-Msum(list(X,Y))
sum

#Example 2:
data(XX)
X<-translator(XX[[1]],50)
Y<-translator(XX[[2]],50)
Z<-translator(XX[[3]],50)
YY<-list(X,Y,Z)
M<-Msum(YY)
M


## The function is currently defined as
function (XX, pic = 0) 
{
    k <- length(XX)
    temp <- rep(0, k)
    for (i in 1:k) {
        temp[i] <- checking(XX[[i]], 0)
    }
    ok <- 1
    if (min(temp) == 0) {
        print(paste("One or more elements of the list"))
        print(paste("don't define a polygonal fuzzy number"))
        print(paste("use checking function to specify the violations"))
        print(paste("in the definition of a poligonal fuzzy number"))
        ok <- 0
    }
    if (min(temp) == 1) {
        if (length(XX) == 1) {
            R <- XX[[1]]
        }
        if (length(XX) >= 2) {
            number <- rep(0, k)
            for (i in 1:k) {
                number[i] <- nrow(XX[[i]])
            }
            if (max(number) != min(number)) {
                print(paste("use translator function to assure that"))
                print(paste("list elements are compatible (same alpha levels"))
                ok <- 0
            }
            if (max(number) == min(number)) {
                equal <- rep(0, k - 1)
                for (i in 1:(k - 1)) {
                  equal[i] <- max(abs(XX[[1]]$alpha - XX[[i]]$alpha))
                }
                if (max(equal) > 0) {
                 print(paste("use translator function to assure that"))
                 print(paste("list elements are compatible (same alpha levels"))
                  ok <- 0
                }
            }
        }
        if (length(XX) >= 2 & ok == 1) {
            R <- XX[[1]]
            for (i in 2:k) {
                R$x <- R$x + XX[[i]]$x
            }
        }
    }
    if (ok == 1) {
        if (pic == 1) {
            lower <- rep(0, k)
            upper <- lower
            for (j in 1:k) {
                lower[j] <- min(XX[[j]])
                upper[j] <- max(XX[[j]])
            }
            limx <- c(min(c(lower, R$x)) - 0.25, max(c(upper, 
                R$x)) + 0.25)
            plot(XX[[1]], type = "l", xlim = limx, lwd = 0.3, 
                xlab = NA, ylab = expression(alpha), cex.main = 1, col = "gray", 
                main = paste("Sum (black) of fuzzy numbers (gray)", 
                  sep = ""))
            for (j in 2:k) {
                lines(XX[[j]], type = "l", lwd = 0.3, col = "gray")
            }
            lines(R, type = "l", lwd = 2, col = "black")
        }
        invisible(R)
    }
  }
}
\keyword{ arith }
\keyword{ manip }

