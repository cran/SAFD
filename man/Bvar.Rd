\name{Bvar}
\Rdversion{1.1}
\alias{Bvar}
\title{
(Sample) Variance
}
\description{
The sample variance of a sample of polygonal fuzzy numbers with respect to the Bertoluzza distance is calculated. Given a list \code{XX} of polygonal fuzzy numbers the function first checks (1) if each element of the list is in the correct form (tested by \code{checking}) and (2) if the alpha-levels of all elements in the list coincide. If these conditions are fulfilled the Bertoluzza sample variance will be returned (i.e. the average Bertoluzza distance of the elements of \code{XX} to its mean). If not the \code{translator} function can be used to transform the elements of the list in the correct format. For details see [1] from below, and replace the kernel K with the expression induced by the Bertoluzza metric. The parameter \code{theta} has to fulfill \code{theta}>0.
}
\usage{
Bvar(XX, theta = 1/3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...should be a list of polygonal fuzzy numbers (the functions implicitly checks the conditions) verifying the above mentioned conditions 
}
  \item{theta}{
...numeric and >0, see \code{bertoluzza}
}
}
\details{
See examples
}
\value{
Given input \code{XX}in the correct format the function returns the Bertoluzza variance of the sample \code{XX}.
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
See Also as \code{\link{bertoluzza}}, \code{\link{Mmean}},\code{\link{Bcov}}, \code{\link{Bcor}}
}
\examples{
#Example 1:
data(XX)
X<-translator(XX[[1]],50)
Y<-translator(XX[[2]],50)
Z<-translator(XX[[3]],50)
YY<-list(X,Y,Z)
A<-Bvar(YY,1)
A

#Example 2:
data(XX)
v<-Bvar(list(XX[[1]],XX[[1]]),1/3)
v

#Example 3:
X<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0.3,0,1,0,0.3)))
Y<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.3,1,0,0.3)))
Z<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,1,1,0)))
ZZ<-list(X,Y,Z)
v<-Bvar(ZZ,1)
v

#Example 4
data(XX)
A<-Bvar(XX,1)
A

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ univar }
