\name{Msum}
\Rdversion{1.1}
\alias{Msum}
\title{
Minkowski sum
}
\description{
Given a list \code{XX} of polygonal fuzzy numbers the function first checks (1) if each element of the list is in the correct form (tested by \code{checking}) and (2) if the alpha-levels of all elements in the list coincide. If these two conditions are fulfilled the levelwise Minkowski-sum of all elements in the sample \code{XX} will be returned. If not the \code{translator} function can be used to transform the elements of the list in the correct format. 
}
\usage{
Msum(XX)
}
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers (the function implicitly checks the conditions) 
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
See Also \code{\link{checking}}, \code{\link{Mmean}}, \code{\link{translator}} 
}
\examples{
#
data(XX)
X<-translator(XX[[1]],50)
Y<-translator(XX[[2]],50)
Z<-translator(XX[[3]],50)
YY<-list(X,Y,Z)
M<-Msum(YY)
M

#
X<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0.3,0,1,0,0.3)))
Y<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.3,1,0,0.3)))
Z<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,1,1,0)))
ZZ<-list(X,Y,Z)
M<-Msum(ZZ)
}

\keyword{ arith }
\keyword{ manip }
