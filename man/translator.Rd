\name{translator}
\Rdversion{1.1}
\alias{translator}
\title{
Translator
}
\description{
The function first calls \code{checking2} in order to check if the input data \code{X} defines a polygonal fuzzy number. If all conditions are satisfied the function transforms the input \code{X} into a dataframe with the chosen number \code{nl} of levels (default is \code{nl}=101) by doing linear interpolation, and returns this dataframe.
}
\usage{
translator(X, nl = 101)
}
\arguments{
  \item{X}{
  ...dataframe for which \code{checking2} yields 1
}
  \item{nl}{
...number of levels of the output dataframe (fuzzy number), by default 101, must be at least 2.
}
}
\details{
See examples
}
\value{
 \item{comp1 }{In case checking2(X)=1 \code{translator} returns a dataframe (fuzzy number) with \code{nl} number of alpha-levels, otherwise the input is returned unchanged.}
}
\references{
}
\author{
Wolfgang Trutschnig <wolfgang.trutschnig@softcomputing.es>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{checking2}}, \code{\link{checking}}
}
\examples{
#example1:
X<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.3,1,0,0.3)))
E<-translator(X,4)

#example2
X<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,1,1,0)))
E<-translator(X,3)
plot(X,type="l")
lines(E,type="l",col="red")

#example3
data(XX)
E<-translator(XX[[3]],11)
plot(XX[[3]],type="l")
lines(E,type="l",col="red")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ attribute }
\keyword{ classes }
