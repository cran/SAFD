\name{Bcor}
\Rdversion{1.1}
\alias{Bcor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 (Sample) correlation
}
\description{
The sample correlation of two samples of polygonal fuzzy numbers with respect to the Bertoluzza distance is calculated. Given lists \code{XX}, \code{YY} of polygonal fuzzy numbers the functions first checks (1) if each element of the two lists is in the correct form (tested by \code{checking}), (2) if the alpha-levels of all elements in the two lists coincide and (3) if the lists have the same length. If all conditions are fulfilled the Bertoluzza correlation will be returned. If not, NA will be returned - in this case the \code{translator} function can be used to transform the elements of the lists in the correct format. For details see [1] from below, and replace the kernel K with the expression induced by the Bertoluzza metric. The parameter \code{theta} has to fulfill \code{theta}>0, by default \code{theta}=1/3.
}
\usage{
Bcor(XX, YY, theta = 1/3)
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
See examples. The code makes use of \code{Bcov} and \code{Bvar}.
}
\value{
Given input \code{XX}, \code{YY} in the correct format the function returns the Bertoluzza correlation of the samples \code{XX}, \code{YY}.
}
\references{
[1] Gonzalez-Rodriguez, G.; Blanco, A.; Colubi, A.; Lubiano, M.A.:
     \emph{Estimation of a simple linear regression model for fuzzy random variables}
       Fuzzy Sets and Systems, 160(3), pp. 357-370 (2009)
}
\author{
Wolfgang Trutschnig <wolfgang.trutschnig@softcomputing.es>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}

\seealso{
See Also as \code{\link{checking}}, \code{\link{Bcov}}, \code{\link{Bvar}}, \code{\link{bertoluzza}} 
}
\examples{
#Example 1:
data(XX)
V<-translator(XX[[3]],100)
XX<-list(length=10)
YY<-XX
  for(i in 1:10){
   XX[[i]]<-generator(V,,,)
   YY[[i]]<-XX[[i]]
   YY[[i]]$x<-5*YY[[i]]$x+1
   }
Bcor(XX,YY,1/3)

#Example 2:
data(XX)
V<-translator(XX[[3]],51)
XX<-list(length=200)
YY<-XX
  for(i in 1:200){
   XX[[i]]<-generator(V,,,)
   YY[[i]]<-XX[[i]]
   YY[[i]]$x<-5*YY[[i]]$x+1
   }
Bcor(XX,YY,1/3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ univar }
