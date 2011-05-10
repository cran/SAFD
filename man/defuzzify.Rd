\name{defuzzify}
\Rdversion{1.1}
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
\references{
%% ~put references to the literature/web site here ~
}
\author{
Wolfgang Trutschnig <wolfgang.trutschnig@softcomputing.es>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(XX)
V<-translator(XX[[3]],50)
V2<-V
V2$x<-V$x/20
SS<-list(length=150)
for (j in 1:150){
 SS[[j]]<-generator(V2,)
 }
a<-defuzzify(SS)
a

#
X<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0.3,0,1,0,0.3)))
Y<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.3,1,0,0.3)))
Z<-data.frame(cbind(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,1,1,0)))
FF<-list(Z,X,Y)
a<-defuzzify(FF)
a

RR<-vector("list",length=1503)
RR[1:3]<-FF
RR[4:1503]<-SS
a<-defuzzify(RR)
a

RR<-vector("list",length=1501)
RR[1]<-FF[1]
RR[2:1501]<-SS
a<-defuzzify(RR)
a
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ datagen }% __ONLY ONE__ keyword per line
