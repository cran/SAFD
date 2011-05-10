\name{Fmedian}
\alias{Fmedian}
\title{
Median of a fuzzy sample
}
\description{
 Given a list \code{XX} of polygonal fuzzy numbers the functions first checks (1) if each element of the lists is in the correct form (tested by \code{checking}) and (2) if the alpha-levels of all elements coincide. If all conditions are fulfilled the function calculates the (levelwise) median (which, by definition, is a fuzzy number too).
}
\usage{
Fmedian(XX, pic = 1)
}
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers with the same alpha levels (the functions implicitly checks the conditions)
}
  \item{pic}{
...numeric, if \code{pic}==1 the sample, its mean and its median are plotted.
}
}
\details{
See examples.
}
\value{
Given correct input \code{XX} the function returns a the median of the sample.
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See Also as \code{\link{Fquantile}} 
}
\examples{
data(XX)
V<-translator(XX[[3]],100)
YY<-vector("list",length=50)
  for(i in 1:50){
   YY[[i]]<-generator(V,,,)
   }
Q<-Fmedian(YY)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ quantiles }
\keyword{ ecdf }
