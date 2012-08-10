\name{btest2.mean}
\Rdversion{1.1}
\alias{btest2.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Two-sample bootstrap test on the equality of mean of two FRVs
}
\description{
Given two samples \code{XX} and \code{YY} of polygonal fuzzy numbers the function first checks if each element of \code{XX} and \code{YY} has the correct format and if the alpha-levels of all input fuzzy numbers coincide. In case yes, the function compute the test statistic described in [1] below. Before doing the resampling \code{Mmean(YY)} is added to each element of \code{XX} and vice versa. Based on these two new samples \code{B} values of the test statistic are calculate. The returned p-value is calculated as the portion of the obtained values of the bootstrap statistic that are greater than the value of the test-statistic. If \code{pic}=1 then the sample means of \code{XX} and \code{YY} are plotted in one window and the ecdf of the bootstrap statistic in another one, otherwise no plot is produced. For detailed explanation see the papers [1] and [2] below.
}
\usage{
btest2.mean(XX, YY, B = 1000, pic = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...should be a list of polygonal fuzzy numbers (the functions implicitly checks the conditions) 
}
  \item{YY}{
...should be a list of polygonal fuzzy numbers (the functions implicitly checks the conditions) 
}
  \item{B}{
...integer, by default \code{B}=1000.
}
  \item{pic}{
...numeric, if \code{pic}=1 then the sample means of \code{XX} and \code{YY} are plotted in one window and the ecdf of the bootstrap statistic in another one. By default \code{pic}=1.
}
}
\details{
See examples
}
\value{
Given input \code{XX} and \code{YY} in the correct format, the function returns the p-value of the two-sided bootstrap test.
}
\references{
[1] Colubi, A.: \emph{Statistical inference about the means of fuzzy random variables: Applications to the analysis of fuzzy- and real-valued data}, Fuzzy Sets and Systems, 160(3), pp. 344-356 (2009) 
      
[2] Montenegro, M., Casals, M.R., Lubiano, M.A., Gil, M.A.: \emph{Two-sample hypothesis tests of means of a fuzzy random variable}, Information Sciences, Vol. 133(1-2), pp. 89-100 (2001)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
The function is quite slow - we will try to solve this problem in the next versions of the package.\cr
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{Mmean}}, \code{\link{Bvar}}, \code{\link{bertoluzza}}
}
\examples{
#run for bigger B
data(XX)
X<-translator(XX[[1]],20)
Y<-translator(XX[[2]],20)
XX<-vector("list",length=30)
for (j in 1:30){
 XX[[j]]<-generator(X,)
 }
YY<-vector("list",length=20)
for (j in 1:20){
 YY[[j]]<-generator(Y,)
 }
b<-btest2.mean(XX,YY,10)
b

#takes some time in the current version:
#data(Trees)
#b<-btest2.mean(Trees[[1]],Trees[[2]],50)
#b
#b<-btest2.mean(Trees[[1]],Trees[[3]],50)
#b
}

\keyword{ htest }
\keyword{ univar }
