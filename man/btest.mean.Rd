\name{btest.mean}
\alias{btest.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
One-sample bootstrap test for the mean of a FRV
}
\description{
Given a sample \code{XX} of polygonal fuzzy numbers and a polygonal fuzzy number \code{V} the function first checks if each element of \code{XX} and \code{V} has the correct format and if the alpha-levels of all input fuzzy numbers coincide. In case yes, the function computes the standardized mean squared Bertoluzza-distance from the sample mean to \code{V} as test-statistic. Afterwards for \code{B} bootstrap samples the (bootstrap) statistic is calculated. The returned p-value is calculated as the portion of the obtained values of the bootstrap statistic that are greater than the value of the test-statistic. Furthermore, if \code{pic}=1 sample mean and \code{V} are plotted in one window and the ecdf of the bootstrap statistic in another one. For detailed explanation see papers [1] and [2] below.
}
\usage{
btest.mean(XX, V, theta = 1/3, B = 100, pic = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{XX}{
...list of polygonal fuzzy numbers (the functions implicitly checks the conditions). 
}
  \item{V}{
...polygonal fuzzy number that is tested to be the mean of the FRV.
}
   \item{theta}{
...numeric and >0, see \code{bertoluzza}
}
  \item{B}{
...integer, by default \code{B}=1000.
}
  \item{pic}{
...numeric, if \code{pic}=1 sample mean and \code{V} are plotted in one window and the ecdf of the bootstrap statistic in another one. By default \code{pic}=1.
}
}
\details{
See examples
}
\value{
Given input \code{XX} and \code{V} in the correct format, the function returns the p-value of the two-sided bootstrap test that the expectation is \code{V}.
}
\references{
[1] Colubi, A.: \emph{Statistical inference about the means of fuzzy random variables: Applications to the analysis of fuzzy- and real-valued data}, Fuzzy Sets and Systems, 160(3), pp. 344-356 (2009)\cr
[2] Montenegro, M., Colubi, A., Casals, M.R., Gil, M.A.: \emph{Asymptotic and bootstrap techniques for testing the expected value of a fuzzy random variable}, Metrika, 59, pp. 31-49  (2004)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
The function is quite slow.\cr
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{Mmean}}, \code{\link{Bvar}}, \code{\link{bertoluzza}},  \code{\link{btest2.mean}}, \code{\link{btestk.mean}}
}
\examples{
#Example 1: run for bigger sample sizes:
data(XX)
V<-translator(XX[[3]],50)
V2<-V
SS<-vector("list",length=50)
for (j in 1:50){
 SS[[j]]<-generator(V2,)
 }
b<-btest.mean(SS,V2,B=10)
b

#Example 2: takes some time to run:
#data(Trees)
#V<-Trees[[1]][[47]]
#b<-btest.mean(Trees[[1]],V,100)
#b
}

\keyword{ htest }
\keyword{ univar }