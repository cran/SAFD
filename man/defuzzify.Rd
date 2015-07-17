\name{defuzzify}
\alias{defuzzify}
\title{
Defuzzification
}
\description{
Given a list \code{XX} of polygonal fuzzy numbers the function defuzzifies all elements of the list and returns the vector of Steiner points (as weighting measure the Lebesgue measure on [0,1] is used).
}
\usage{
defuzzify(XX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...should be a list of polygonal fuzzy numbers (the function implicitly checks the conditions) 
}
}
\details{
See examples
}
\value{
Given input \code{XX} in the correct format the function returns vector of Steiner points.
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\examples{
#Example:
data(XX)
V<-translator(XX[[3]],50)
V2<-V
V2$x<-V$x/20
SS<-vector("list",length=150)
for (j in 1:150){
 SS[[j]]<-generator(V2,)
 }
a<-defuzzify(SS)
a


## The function is currently defined as
function (XX) 
{
    temp_sum <- Msum(XX, 0)
    if (is.null(temp_sum) == 0) {
        defuzz <- function(X) {
            nl <- nrow(X)/2
            mids <- (X$x[1:nl] + X$x[(2 * nl):(nl + 1)])/2
            values <- mids[2:nl] + mids[1:(nl - 1)]
            delta <- 1/(nl - 1)
            integral <- sum(values) * delta/2
            return(integral)
        }
        k <- length(XX)
        a <- rep(0, k)
        for (i in 1:k) {
            a[i] <- defuzz(XX[[i]])
        }
        invisible(a)
    }
  }
}
\keyword{ manip }
\keyword{ datagen }
