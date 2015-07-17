\name{hukuhara}
\alias{hukuhara}
\title{
Hukuhara Difference
}
\description{
Given two polygonal fuzzy numbers the functions calculates the Hukuhara difference \code{Y-X} if it exists. First the input data is tested for having the correct format using the function \code{checking}. If the Hukuhara difference exists and \code{pic}=1 then the input and the Hukuhara difference is plotted, otherwise no plot is produced.
}
\usage{
hukuhara(X, Y, pic = 1)
}
\arguments{
  \item{X}{
...polygonal fuzzy number (tested by \code{checking})
}
  \item{Y}{
...polygonal fuzzy number (tested by \code{checking})
}
  \item{pic}{
...numeric, if \code{pic}=1 then \code{X}, \code{Y} and \code{Y-X} (if existing) is plotted
}
}
\details{
See examples
}
\value{
In case the input data is in the correct form and the Hukuhara difference exists, the Hukuhara difference is returned, in case not, NA is returned.
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}

\seealso{
See Also as \code{\link{checking}}, \code{\link{Msum}}, \code{\link{lrmodel}}
}
\examples{
#Example 1:
Y<-data.frame(x=c(0,0,0,1,2,2),alpha=c(0,0.5,1,1,0.5,0))
X<-data.frame(x=c(0,0,0,0,1.5,2),alpha=c(0,0.5,1,1,0.5,0))
Z<-data.frame(x=c(0,0,0,0.75,1.5,1.5),alpha=c(0,0.5,1,1,0.5,0))
h1<-hukuhara(X,Y,1)
h1
h2<-hukuhara(Z,Y,1)
h2

#Example 2: in this case the hukuhara diff has to exist by construction
data(XX)
X<-translator(XX[[1]],50)
shift<-seq(-1,1,length=100)
Y<-X
Y$x<-X$x+shift
h<-hukuhara(X,Y,1)
h

#Example 3: in this case the hukuhara diff has to exist by construction
data(XX)
X<-translator(XX[[1]],5000)
shift<-seq(-1,1,length=10000)
Y<-X
Y$x<-X$x+shift
H<-hukuhara(X,Y,1)
H

## The function is currently defined as
function (X, Y, pic = 1) 
{
    temp_sum <- Msum(list(X, Y))
    if (is.null(temp_sum) == 0) {
        nl <- nrow(X)/2
        dif <- data.frame(x = Y$x - X$x, alpha = X$alpha)
        if (pic == 1) {
            plot(X, type = "l", xlim = c(min(c(X$x, Y$x)), max(c(X$x, 
                Y$x))), xlab = " ", ylab = expression(alpha))
            lines(Y, type = "l")
        }
        a <- checking(dif, 0)
        if (a == 0) {
            print("Hukuhara difference Y-X does not exist")
        }
        if (a == 1) {
            if (pic == 1) {
             plot(X, type = "l", xlim = c(min(c(X$x, Y$x)), 
             max(c(X$x, Y$x))), xlab = NA, ylab = expression(alpha), col = "gray", 
             main = paste("Hukuhara difference (black) of fuzzy numbers (gray)", 
                    sep = ""), cex.main = 1)
                lines(Y, type = "l", col = "gray")
                lines(dif, type = "l", lwd = 2)
            }
            invisible(dif)
        }
    }
  }
}
\keyword{ arith }
\keyword{ manip }
