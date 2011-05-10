\name{Fquantile}
\alias{Fquantile}
\title{
Quantiles of a fuzzy sample
}
\description{
 Given a list \code{XX} of polygonal fuzzy numbers the functions first checks (1) if each element of the lists is in the correct form (tested by \code{checking}) and (2) if the alpha-levels of all elements coincide. If all conditions are fulfilled the function calculates the chosen (levelwise) quantiles. As in the basic R-function \code{quantile} a vector \code{p} of quantiles can be calculated. The functions returns a list of quantiles (which are, by definition, fuzzy numbers too).
}
\usage{
Fquantile(XX, p = c(0, 0.25, 0.5, 0.75, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers with the same alpha levels (the functions implicitly checks the conditions)
}
  \item{p}{
...numeric vector of probabilities with values in [0,1]
}
}
\details{
See examples.
}
\value{
Given correct input \code{XX} the function returns a  list of quantiles.
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
See Also as \code{\link{Fmedian}}, \code{\link{Mmean}} 
}
\examples{
data(XX)
V<-translator(XX[[3]],100)
YY<-vector("list",length=50)
  for(i in 1:50){
   YY[[i]]<-generator(V,,,)
   }
QQ<-Fquantile(YY,)
QQ<-Fmedian(YY)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ quantiles }
\keyword{ ecdf }
