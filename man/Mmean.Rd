\name{Mmean}
\Rdversion{1.1}
\alias{Mmean}
\title{
Minkowski mean
}
\description{
Given a sample \code{XX} of polygonal fuzzy numbers the Minkowski-mean of the sample is calculated. The function fist calls \code{Msum} to check if \code{XX} has the correct format and, in case yes, \code{sc_mult} to calculate the Minkowski-mean of the fuzzy sample \code{XX}. If \code{pic}=1 then the sample and its mean are plotted, otherwise no plot is produced.
}
\usage{
Mmean(XX, pic = 0)
}
\arguments{
  \item{XX}{
...should be a list of polygonal fuzzy numbers (the functions implicitly checks the conditions) 
}
  \item{pic}{
...numeric, if \code{pic}=1 then the sample mean of \code{XX} is printed. By default \code{pic}=0.
 }
}
\details{
See examples
}
\value{
Given input \code{XX} in the correct format the function returns the Minkowski mean of the polygonal fuzzy numbers contained in the list \code{XX}. 
}
\references{
%
}
\author{
Wolfgang Trutschnig <wolfgang.trutschnig@softcomputing.es>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{checking}}, \code{\link{Msum}},  \code{\link{sc_mult}},\code{\link{translator}} 
}
\examples{
data(XX)
A<-Mmean(XX,1)
X<-translator(XX[[1]],50)
Y<-translator(XX[[2]],50)
Z<-translator(XX[[3]],50)
YY<-list(X,Y,Z)
A<-Mmean(YY,pic=1)
#
X<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0.3,0,1,0,0.3)))
Y<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.3,1,0,0.3)))
Z<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,1,1,0)))
ZZ<-list(X,Y,Z)
Mmean(ZZ)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ univar }
\keyword{ manip }
