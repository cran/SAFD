\name{DSfrequency}
\alias{DSfrequency}
\title{
Levelwise Dempster-Shafer frequency
}
\description{
Given a sample \code{XX} of polygonal fuzzy numbers and an interval \code{IV} the levelwise Dempster-Shafer frequency of the interval is calculated, i.e. for the chosen number \code{nl} of equidistant alpha-cuts it is checked how many of the elements of the sample have an alpha-cut that is contained in the interval (lower frequency) and how many have an alpha-cut hitting the interval (upper frequency). These family of intervals is afterwards aggregated to another polygonal fuzzy number with the corresponding number of alpha-cuts, which is returned. For details see [1] below. Preliminary the input data is tested for the correct format using the \code{checking} function.
}
\usage{
DSfrequency(XX, IV = c(0, 1), pic = 1, nl = 101)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers (the functions implicitly checks the conditions) 
}
  \item{IV}{
...numeric vector of length two, by default \code{IV}=c(0,1)
}
  \item{pic}{
...numeric, in case \code{pic}=1 the frequency is plotted, otherwise no plot is produced
}
  \item{nl}{
...number of equidistant alpha-levels, by default \code{nl}=101
}
}
\details{
See examples
}
\value{
Given correct input data, the function returns the levelwise Dempster-Shafer frequency of the chosen interval (again in the correct form of a polygonal fuzzy number).
}
\references{
[1] Trutschnig, W., \emph{A strong consistency result for fuzzy relative frequencies interpreted as estimator for the fuzzy-valued probability}, Fuzzy Sets and Systems, Vol. 159, nr 3, pp. 259-269 (2008) \cr
[2] Viertl, R., Hareter, D.: \emph{Beschreibung und Analyse unscharfer Information: Statistische Methoden fuer unscharfe Daten}, Springer Wien New York, 2006 
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{checking2}}, \code{\link{translator}}, \code{\link{DShistogram}}
}
\examples{
# Example:
data(XX)
V<-XX[[1]]
SS<-vector("list",length=500)
for (j in 1:500){
 SS[[j]]<-generator(V,)
 }
A<-DSfrequency(SS,c(1,3),1,100)
head(A)
}

\keyword{ manip }
\keyword{ datagen }
