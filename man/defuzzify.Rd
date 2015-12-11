\name{defuzzify}
\alias{defuzzify}
\title{
Defuzzification
}
\description{
Given a list \code{XX} of polygonal fuzzy numbers the function defuzzifies all elements of the list and returns the vector of Steiner points (as weighting measure the Lebesgue measure on [0,1] is used).
}
\usage{
defuzzify(XX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...should be a list of polygonal fuzzy numbers (the function implicitly checks the conditions) 
}
}
\details{
See examples
}
\value{
Given input \code{XX} in the correct format the function returns vector of Steiner points.
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\examples{
#Example:
data(XX)
V<-translator(XX[[3]],50)
V2<-V
V2$x<-V$x/20
SS<-vector("list",length=150)
for (j in 1:150){
 SS[[j]]<-generator(V2,)
 }
a<-defuzzify(SS)
a
}

\keyword{ manip }
\keyword{ datagen }