\name{Fmedian}
\alias{Fmedian}
\title{
Median of a fuzzy sample 
}
\description{
Given a list \code{XX} of polygonal fuzzy numbers the functions first checks (1) if each element of the lists is in the correct form (tested by \code{checking}) and (2) if the alpha-levels of all elements coincide. If all conditions are fulfilled the function calculates the (levelwise) median (which, by definition, is a fuzzy number too) using a large number of levels, by default \code{nl}=101.
}
\usage{
Fmedian(XX, nl = 101, pic = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers with the same alpha levels (the functions implicitly checks the conditions)
}
  \item{nl}{
...number of equidistant alpha-level, by default \code{nl}=101
}
 \item{pic}{
...numeric, if \code{pic}=1 the sample, its mean and its median are plotted.
}
}
\details{
See examples.
}
\value{
Given correct input \code{XX} the function returns the median of the sample.
}
\references{
[1] Sinova, B., Gil, M.A., Colubi, A., Van Aelst, S.: \emph{The median of a random fuzzy number. The 1-norm distance approach}, Fuzzy Sets and Systems, 200, pp. 99-115 (2012)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{Mmean}} 
}
\examples{
#Example 1:
data(XX)
V<-translator(XX[[3]],100)
YY<-vector("list",length=50)
  for(i in 1:50){
   YY[[i]]<-generator(V,,,)
   }
Me<-Fmedian(YY)

#Example 2:
data(Trees)
Species1_Median<-Fmedian(Trees[[1]],nl=11)
Species1_Median
Species2_Median<-Fmedian(Trees[[2]])
Species3_Median<-Fmedian(Trees[[3]])
}

\keyword{ univar }
\keyword{ manip }
