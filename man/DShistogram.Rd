\name{DShistogram}
\Rdversion{1.1}
\alias{DShistogram}
\title{
Levelwise Dempster-Shafer Histogram
}
\description{
Based on a sample \code{XX} of polygonal fuzzy numbers (tested by \code{checking}), a chosen interval \code{limx}, a chosen number \code{npart} of partitions elements and a chosen number \code{nl} of equidistant alpha-levels the levelwise Dempster-Shafer frequency for each partition element is calculated. If \code{pic}=TRUE then a 3d plot as well as an image-plot of the histogram is produced. In case of \code{pdf}=TRUE a pdf containing these plots is produced automatically. 
}
\usage{
DShistogram(XX, limx = NA, npart = 10, nl = 101, pic = TRUE, pdf = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers (the function implicitly checks the conditions) 
}
  \item{limx}{
...numeric vector of length two, by default limx=c(0,1), that determines the x-range for which the histogram is plotted 
}
  \item{npart}{
...integer, number of partitions elements
}
  \item{nl}{
...number of equidistant alpha-level, by default \code{nl}=101
}
  \item{pic}{
...if \code{pic}=1, a 3d- and an image-plot of the histogram is produced
}
  \item{pdf}{
...if \code{pdf}=1, a 3d- and an image-plot of the histogram is automatically exported as pdf, by default \code{pdf}=FALSE
}
}
\details{
See examples
}
\value{
If the input data is in the correct form the function returns a list with the following elements:
 \item{gridx}{x-grid for plotting the histogram}
 \item{gridy}{y-grid for plotting the histogram}
 \item{M}{values of the histogram as function on the grid}
 \item{breaks}{breaks of the histogram}
}
\references{
[1] Trutschnig, W., \emph{A strong consistency result for fuzzy relative frequencies interpreted as estimator for the fuzzy-valued probability}, Fuzzy Sets and Systems, Vol. 159, nr 3, pp. 259-269 (2008)\cr
[2] Viertl, R., Hareter, D.: \emph{Beschreibung und Analyse unscharfer Information: Statistische Methoden fuer unscharfe Daten}, Springer Wien New York, 2006 
}
\author{
Wolfgang Trutschnig <wolfgang.trutschnig@softcomputing.es>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
Speed to be improved in future versions of the package,\cr
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{DSfrequency}}
}
\examples{
#an example with a random variable with small spread of the expectation
#run for bigger sample size and finer partition
data(XX)
V<-translator(XX[[3]],30)
V2<-V
V2$x<-V$x/5
SS<-vector("list",length=300)
for (j in 1:300){
 SS[[j]]<-generator(V2,)
 }
A<-DShistogram(SS,c(-3,3),npart=6,nl=51)

#run for bigger sample size and finer partition
data(XX)
V<-translator(XX[[3]],30)
V2<-V
V2$x<-V$x/10
pertV<-list(dist="unif",par=c(-2,2))
SS<-vector("list",length=300)
for (j in 1:300){
 SS[[j]]<-generator(V2,pertV,)
 }
A<-DShistogram(SS,,npart=5,nl=51)

#takes some time but produces nice result
#data(XX)
#V<-translator(XX[[3]],30)
#V2<-V
#V2$x<-V$x/10
#pertV<-list(dist="unif",par=c(-2,2))
#pertL<-list(dist="lnorm",par=c(-2,2))
#SS<-vector("list",length=1000)
#for (j in 1:1000){
# SS[[j]]<-generator(V2,pertV,pertL,)
# }
#A<-DShistogram(SS,,npart=15,nl=51)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
\keyword{ datagen }
