\name{bertoluzza}
\alias{bertoluzza}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bertoluzza distance
}
\description{
Given two polygonal fuzzy numbers \code{X}, \code{Y} in the correct format (testing by \code{checking}) the function calculates the Bertoluzza distance of \code{X}, \code{Y}. The parameter \code{theta} (being the weight of the spread) has to fulfill \code{theta}>0, by default \code{theta}=1/3 (which corresponds to the Lebesgue measure as weighting measure on [0,1]). For detailed explanation see the papers [1] and [2] below.
}
\usage{
bertoluzza(X, Y, theta = 1/3, pic = 0)
}
\arguments{
  \item{X}{
...dataframe (polygonal fuzzy number)
}
  \item{Y}{
...dataframe (polygonal fuzzy number)
}
  \item{theta}{
...numeric and >0
}
  \item{pic}{
...numeric, if \code{pic}==1 \code{X} and \code{Y} are plotted, by default no plot is produced. 
}
}
\details{
See examples
}
\value{
...in case \code{X} and \code{Y} are in the correct form the code returns the Bertoluzza distance, otherwise NA is returned.
}
\references{
[1] Trutschnig, W., Gonzalez-Rodriguez, G., Colubi, A., Gil, M.A.: \emph{A new family of metrics for compact, convex (fuzzy) sets based on a generalized concept of mid and spread},  Information Sciences, 179(23), pp. 3964-3972 (2009) 
     
[2] Bertoluzza, C., Corral, N., Salas, A.: \emph{On a new class of distances between fuzzy numbers}, Mathware Soft Comput., 2, pp:71-84 (1995)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{checking}}, \code{\link{Mmean}}, \code{\link{Bvar}}
}
\examples{
#Example 1:
data(XX)
X<-translator(XX[[1]],50)
Y<-translator(XX[[2]],50)
Z<-translator(XX[[3]],50)
ZZ<-list(X,Y,Z)
b<-bertoluzza(X,Y,1/3,1)
b

#Example 2: example (SLLN for the FRV)
V<-translator(XX[[3]],100)
YY<-vector("list",length=50)
  for(i in 1:50){
   YY[[i]]<-generator(V,,,)
   }
M<-Mmean(YY)
head(M)
b<-bertoluzza(M,V,1/3,1) 
b

#Example 3:
V<-translator(XX[[3]],100)
YY<-vector("list",length=1000)
  for(i in 1:1000){
   YY[[i]]<-generator(V,,,)
   }
M<-Mmean(YY)
head(M)
b<-bertoluzza(M,V,1/3,1)  
b

#Example 4:
X<-data.frame(x=c(0,1,1,2),alpha=c(0,1,1,0))
Y<-data.frame(x=c(0,1,2),alpha=c(0,1,0))
b<-bertoluzza(X,Y,1/3,1)
b

#Example 5:
data(Trees)
X<-Mmean(Trees[[1]])
Y<-Mmean(Trees[[2]])
Z<-Mmean(Trees[[3]])
b1<-bertoluzza(X,Y,1)
b1
b2<-bertoluzza(X,Z,1)
b2
b3<-bertoluzza(Y,Z,1)
b3
}

\keyword{ math }
\keyword{ manip }
