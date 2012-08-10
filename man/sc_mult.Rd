\name{sc_mult}
\Rdversion{1.1}
\alias{sc_mult}
\title{
Minkowski scalar multiplication
}
\description{
Given an input dataframe (polygonal fuzzy number) \code{X} in the correct format (tested by \code{checking}), and a scalar \code{b} the fuzzy number \code{bX} is calculated using level-wise Minkowski scalar multiplication.
}
\usage{
sc_mult(X, b)
}
\arguments{
  \item{X}{
...dataframe, if checking(X)=1 the product \code{b}\code{X} is returned, if not, NA is returned.
}
  \item{b}{
...numeric
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
See Also \code{\link{checking}}
}
\examples{
data(XX)
X<-translator(XX[[1]],10)
E<-sc_mult(X,-2)
E


U<-data.frame(cbind(x=c(-1,0,1),alpha=c(0,1,0)))
E<-sc_mult(U,2)
E
}

\keyword{ arith }
\keyword{ manip }
