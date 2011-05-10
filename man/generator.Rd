\name{generator}
\Rdversion{1.1}
\alias{generator}
\title{
 Simulation of fuzzy random variables (FRV)
}
\description{
The second procedure described in [1] is implemented. Given an input dataframe \code{V} in the correct format (tested by \code{checking}), which will be the expectation of the simulated FRV first \code{decomposer(V)} is called. Loosely speaking, the dataframe \code{Y} returned by \code{decomposer}, which contains the "coordinates" of \code{V} with respect to a certain "basis" (see [1]), is perturbated stochastically in order to generate a new polygonal fuzzy number. The distributions used for these perturbations can be selected in the call of the function, however, in this Version 0.1 only a few choices are possible: (1) The perturbation of the centre of the 1-cut \code{pertV} has to be of the form \code{norm(0,sigma)} or \code{unif(-a,a)}, sigma,a>0. (2) The perturbation of the left part of the fuzzy set \code{pertL} has to be of the form \code{chisq(1)}, \code{exp(1)} or \code{lnorm(a,b)} with expectation=1. (3) The perturbation of the right part of the fuzzy set \code{pertR} has to be of the same form as that for the left part.
}
\usage{
generator(V, pertV = list(dist = "norm", par = c(0, 1)), 
   pertL = list(dist = "chisq", par = c(1)), 
   pertR = list(dist = "chisq", par = c(1)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{
...polygonal fuzzy set in the correct format (tested by \code{checking})
}
  \item{pertV}{
...list containing elements "dist" and "par". "dist" denotes the chosen distribution family (normal or uniform) and "par" the corresponding parameters.
}
  \item{pertL}{
...list containing elements "dist" and "par". "dist" denotes the chosen distribution family (chisq or lnorm) and "par" the corresponding parameters.
}
  \item{pertR}{
...list containing elements "dist" and "par". "dist" denotes the chosen distribution family (chisq or lnorm) and "par" the corresponding parameters.
}
}
\details{
See examples
}
\value{
Given correct input data, the function returns a polygonal fuzzy number that can be seen as a realisation of a FRV with expectation \code{V} (see [1]).
}
\references{
[1] Gonzalez-Rodriguez, G., Colubi, A., Trutschnig, W.: \emph{Simulation of fuzzy random variables},  Information Sciences, 179(5), pp. 642-653 (2009)
}
\author{
Wolfgang Trutschnig <wolfgang.trutschnig@softcomputing.es>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{decomposer}}, \code{\link{checking}}
}
\examples{
data(XX)
V<-translator(XX[[3]],100)
YY<-vector("list",length=100)
  for(i in 1:100){
   YY[[i]]<-generator(V,,,)
   }
M<-Mmean(YY) 
M
plot(M,type="l",xlim=c(-3,4))
lines(V,type="l",col="red",lwd=2)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ nonparametric }
\keyword{ datagen }
