\name{checking2}
\Rdversion{1.1}
\alias{checking2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checking correct data format  (weak version)
}
\description{
The function checks if the input data defines a polygonal fuzzy number, i.e. a dataframe with the columns "x" and "alpha" fulfilling the following conditions: (1) alpha-values have to be in [0,1] with the minimum alpha-level being 0 and maximum being 1, (2) the x-values have to be non-decreasing, (3) the alpha-levels have to increase from 0 to 1 and afterwards decrease from 1 to 0 (not necessarily in the same way). The function is only used for the \code{translator} function.
}
\usage{
checking2(X, com = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  ...can be any data frame.
}
  \item{com}{
 ...numeric, if \code{com}=1 then, in case of an error, a comment is printed. By default \code{com}=1.
}
}
\details{
See examples
}
\value{
The function returns the value 1 if the input fulfills all conditions, if not, 0 is returned. 
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}

\seealso{
 See Also as \code{\link{checking}}, \code{\link{translator}}
}
\examples{
 U<-data.frame(cbind(x=c(-1,0,1),alpha=c(0,1,0)))
 #a<-checking(U,)
 a<-checking2(U,)
a
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ attribute }
\keyword{ classes }
