\name{btestk.mean}
\alias{btestk.mean}
\title{
Multi-sample bootstrap test for the equality of the mean of FRVs
}
\description{
Given a list \code{XXX} of length \code{k} sublists of polygonal fuzzy numbers the function first checks if each element of the sublists has the correct format and if the alpha-levels of all input fuzzy numbers coincide. The vector \code{sel} contains the numbers of the sublists the user wants to filter to. After filtering the relevant part of \code{XXX} the function computes the test-statistic, which compares the sum of the distances of the groups means and the overall mean with the sum of the group variances. Before doing the resampling \code{length(sel)} new samples are calculated by adding to each element of every fixed group the sum of all means of the other groups. Based on these \code{length(sel)} new samples \code{B} values of the (bootstrap) test statistic are calculate. The returned p-value is calculated as the portion of the obtained values of the bootstrap statistic that are greater than the value of the test-statistic. If \code{pic}=1 then the sample means of the via \code{sel} selected samples from \code{XXX} and the total mean are plotted, otherwise no plot is produced. For a more detailed explanation see the papers [1] and [2] below.
}
\usage{
btestk.mean(XXX, sel, theta = 1/3, B = 100, pic = 1)
}
\arguments{
  \item{XXX}{
...  A list of sublists, each of which contains polygonal fuzzy numbers
}
  \item{sel}{
...vector, selection of number of the samples (sublists) to be considered
}
  \item{theta}{
...numeric and >0
}
 \item{B}{
...integer, by default \code{B}=100.
}
  \item{pic}{
...numeric, if \code{pic}=1 then the sample means of the via \code{sel} selected samples from \code{XXX} and the total mean are plotted. By default \code{pic}=1.
}
}
\details{
See examples
}
\value{
Given input \code{XXX} in the correct format, the function returns the p-value of the two-sided test.
}
\references{
[1] Colubi, A.: \emph{Statistical inference about the means of fuzzy random variables: Applications to the analysis of fuzzy- and real-valued data}, Fuzzy Sets and Systems, 160(3), pp. 344-356 (2009)
     
[2] Gil, M.A.; Montenegro, M.; Gonzalez-Rodriguez, G.; Colubi, A.; Casals, R.: \emph{Bootstrap approach to the multi-sample test of means with imprecise data}, Computational Statistics and Data Analysis, 51(1), pp. 148-162  (2006)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
The function is quite slow.\cr
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{Mmean}}, \code{\link{Bvar}}, \code{\link{bertoluzza}},  \code{\link{btest1.mean}}, \code{\link{btest2.mean}}
}
\examples{
#Example 1: very small B only for testing purpose
data(Trees)
sel<-c(1,2,3)
b<-btestk.mean(Trees,sel,B=5)
b

#Example 2: run for bigger B
#b<-btestk.mean(Trees,sel,100)
#b
}

\keyword{htest}
\keyword{univar}