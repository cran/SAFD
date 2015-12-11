\name{sc_mult}
\alias{sc_mult}
\title{
Minkowski scalar multiplication
}
\description{
Given an input dataframe (polygonal fuzzy number) \code{X} in the correct format (tested by \code{checking}), and a scalar \code{b} the fuzzy number \code{bX} is calculated using level-wise Minkowski scalar multiplication.
}
\usage{
sc_mult(X, b, pic = 0)
}
\arguments{
\item{X}{
...dataframe, if checking(X)=1 the product \code{b}\code{X} is returned, if not, NA is returned.
}
  \item{b}{
...numeric
}
  \item{pic}{
...numeric, if \code{pic}=1 then the product of \code{X} by the scalar \code{b} is printed. By default \code{pic}=0.
}
}
\details{
See examples
}
\value{
Given correct input data, the function returns the polygonal fuzzy number \code{b}\code{X}.

}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also \code{\link{checking}}, \code{\link{translator}}
}
\examples{
#Example 1:
U<-data.frame(x=c(-1,0,1),alpha=c(0,1,0))
E<-sc_mult(U,2)
E

#Example 2:
X<-data.frame(x=c(0,1,1,5),alpha=c(0,1,1,0))
sc_prod<-sc_mult(X,1.5)
sc_prod



#Example 3:
data(XX)
X<-translator(XX[[1]],10)
E<-sc_mult(X,-2,pic=1)
E
}

\keyword{ arith }
\keyword{ manip }