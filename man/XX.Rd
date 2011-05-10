\name{XX}
\Rdversion{1.1}
\alias{XX}
\docType{data}
\title{
Example data
}
\description{
\code{XX} is a list of three polygonal fuzzy numbers that is used in the given examples.   
}
\usage{data(XX)}
\format{
  List of three polygonal fuzzy numbers
}
\details{
See examples
}
\source{
}
\references{
}
\examples{
data(XX)
V<-translator(XX[[1]],50)
SS1<-vector("list",length=100)
for (j in 1:100){
 SS1[[j]]<-generator(V,)
 }
M<-Mmean(SS1,1)
head(M)
b<-bertoluzza(M,V,1/3,1) 
b 
}

\keyword{datasets}
