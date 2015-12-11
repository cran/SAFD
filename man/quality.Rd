\name{quality}
\alias{quality}
\docType{data}
\title{
Quality dataset
}
\description{
\code{quality} is a list with two sublists, each of which consists of a sample of n=10 trapezoidal fuzzy numbers what corresponds to the quality of the lands and the trees within a study about the progress of reforestation in a given area of Asturias (Spain) carried out by the INDUROT institute (University of Oviedo). For details see [1] below and \code{Trees} dataset. 
}
\usage{data("quality")}
\format{
 A list with two sublist, each of which contains trapezoidal fuzzy numbers.
}
\details{
See Reference
}
\references{
[1] Gonzalez-Rodriguez, G.; Blanco, A.; Colubi, A.; Lubiano, M.A.: \emph{Estimation of a simple linear regression model for fuzzy random variables}, Fuzzy Sets and Systems, 160(3), pp. 357-370 (2009) 
}
\examples{
data(quality)
Quality_Cov<-Bcov(quality$trees,quality$land,1/3)
Quality_Cov
model<-lrmodel(quality$land,quality$trees)
model
}
\keyword{datasets}