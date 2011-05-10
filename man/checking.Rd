\name{checking}
\Rdversion{1.1}
\alias{checking}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checking correct data format 
}
\description{
The function checks if the input data is of the correct form of a polygonal fuzzy number, i.e. a dataframe with the columns "x" and "alpha" fulfilling the following conditions: (1) alpha-values have to be in [0,1] with the minimum alpha-level being 0 and maximum being 1, (2) the x-values have to be non-decreasing, (3) the alpha-levels have to increase from 0 to 1 and afterwards decrease from 1 to 0 in the same way (i.e. the alpha-column consists of an increasing vector from 0 to 1 plus the same vector in decreasing order). As a consequence the dataframe always has an even number of rows, see examples. The function is used internally in almost all the other functions to do a preliminary checking if the input data is of the correct form.
}
\usage{
checking(X, com = 1)
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
\references{
}
\author{
Wolfgang Trutschnig <wolfgang.trutschnig@softcomputing.es>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
 See Also as \code{\link{checking2}}, \code{\link{translator}}
}
\examples{
data(XX)
a<-checking(XX[[1]],1)
a
#
X<-data.frame(cbind(y=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,0.9,0.9,0)))
a<-checking(X)
a
#
X<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,0.9,0.9,0)))
a<-checking(X)
a
#
X<-data.frame(cbind(x=c(-2,-0.75,-0.25,-0.5,1),alpha=c(0,0.6,1,1,0)))
a<-checking(X)
a
#
X<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0.3,0,1,0,0.3)))
a<-checking(X)
a
#
Y<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.3,1,0,0.3)))
a<-checking(Y)
a
#
Z<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,1,1,0)))
a<-checking(Z)
a
#
U<-data.frame(cbind(x=c(-1,0,1),alpha=c(0,1,0)))
a<-checking(U,)
a
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ attribute }
\keyword{ classes }
