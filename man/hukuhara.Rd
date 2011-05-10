\name{hukuhara}
\Rdversion{1.1}
\alias{hukuhara}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hukuhara Difference
}
\description{
Given two polygonal fuzzy numbers the functions calculates the Hukuhara difference \code{Y-X} if it exists. First the input data is tested for having the correct format using the function \code{checking}. If the Hukuhara difference exists and \code{pic}=1 then the input and the Hukuhara difference is plotted, otherwise no plot is produced.
}
\usage{
hukuhara(X, Y, pic = 1)
}
\arguments{
  \item{X}{
...polygonal fuzzy number (tested by \code{checking})
}
  \item{Y}{
...polygonal fuzzy number (tested by \code{checking})
}
  \item{pic}{
...numeric, if \code{pic}=1 then \code{X}, \code{Y} and \code{Y-X} (if existing) is plotted
}
}
\details{
See examples
}
\value{
In case the input data is in the correct form and the Hukuhara difference exists, the Hukuhara difference is returned, in case not, NA is returned.
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
See Also as \code{\link{checking}}, \code{\link{lrmodel}}
}
\examples{
#Example 1:
Y<-data.frame(x=c(0,0,0,1,2,2),alpha=c(0,0.5,1,1,0.5,0))
X<-data.frame(x=c(0,0,0,0,1.5,2),alpha=c(0,0.5,1,1,0.5,0))
Z<-data.frame(x=c(0,0,0,0.75,1.5,1.5),alpha=c(0,0.5,1,1,0.5,0))
h<-hukuhara(X,Y,1)
h
h<-hukuhara(Z,Y,1)
h

#Example 2:
Y<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0.3,0,1,0,0.3)))
X<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,1,1,0)))
h<-hukuhara(X,Y)
h

#Example 3: in this case the hukuhara diff has to exist by construction
data(XX)
X<-translator(XX[[1]],50)
shift<-seq(-1,1,length=100)
Y<-X
Y$x<-X$x+shift
h<-hukuhara(X,Y,1)

#Example 4: in this case the hukuhara diff has to exist by construction
data(XX)
X<-translator(XX[[1]],5000)
shift<-seq(-1,1,length=10000)
Y<-X
Y$x<-X$x+shift
H<-hukuhara(X,Y,1)
H
 }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ arith }
\keyword{ manip }
