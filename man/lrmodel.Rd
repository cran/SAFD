\name{lrmodel}
\Rdversion{1.1}
\alias{lrmodel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Linear Regression model 
}
\description{
The linear regression model Y=aX+eps for trapezoidal fuzzy numbers as described in [1] has been implemented, whereby the least-squares-minimization (with constraints) is done with respect to the Bertoluzza metric (with \code{theta}=1/3). Given lists \code{XX}, \code{YY} of polygonal fuzzy numbers the functions first checks (1) if each element of the two list is in the correct form (tested by \code{checking}), (2) if the alpha-levels of all elements in the two lists coincide and (3) if the lists have the same length. If all conditions are fulfilled the function automatically converts the fuzzy numbers in \code{XX} and \code{YY} in trapezoidal ones and returns the estimations for the parameters \code{a} and \code{B} - in case of double solutions both solutions are returned.
}
\usage{
lrmodel(XX, YY)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers (the functions implicitly checks the conditions) having the same length as \code{YY} 
}
  \item{YY}{
...list of polygonal fuzzy numbers (the functions implicitly checks the conditions) having the same length as \code{XX} 
}
}
\details{
See examples
}
\value{
Given input \code{XX}, \code{YY} in the correct format the function returns a list containing the estimates for \code{a} and \code{B} (in case of double solutions both solutions are returned).
}
\references{
[1] Gonzalez-Rodriguez, G.; Blanco, A.; Colubi, A.; Lubiano, M.A.: \emph{Estimation of a simple linear regression model for fuzzy random variables}, Fuzzy Sets and Systems, 160(3), pp. 357-370 (2009) \cr
[2] Gil, M.A., Lopez, M.T., Lubiano, M.A., Montenegro, M.: \emph{Regression and correlation analyses of a linear relation between random intervals}, Test, 10(1), pp. 183-201 (2001)
}
\author{
Wolfgang Trutschnig <wolfgang.trutschnig@softcomputing.es>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{checking}}, \code{\link{Mmean}}, \code{\link{hukuhara}}, \code{\link{Bvar}}, \code{\link{Bcov}}, \code{\link{bertoluzza}}
}
\examples{
#Example 1 (crisp case)
XX<-list(length=2)
XX[[1]]<-data.frame(cbind(x=c(1,1,1,1),alpha=c(0,1,1,0)))
XX[[2]]<-data.frame(cbind(x=c(2,2,2,2),alpha=c(0,1,1,0)))
YY<-list(length=2)
YY[[1]]<-data.frame(cbind(x=c(1,1,1,1),alpha=c(0,1,1,0)))
YY[[2]]<-data.frame(cbind(x=c(2,2,2,2),alpha=c(0,1,1,0)))
lrmodel(XX,YY)

#Example 2:
data(XX)
V<-translator(XX[[3]],100)
XX<-list(length=50)
YY<-XX
  for(i in 1:50){
   XX[[i]]<-generator(V,,,)
   YY[[i]]<-XX[[i]]
   YY[[i]]$x<-5*YY[[i]]$x+1
   }
lrmodel(XX,YY)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ models }
