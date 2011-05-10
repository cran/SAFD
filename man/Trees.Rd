\name{Trees}
\Rdversion{1.1}
\alias{Trees}
\docType{data}
\title{
Tree dataset
}
\description{
\code{Trees} is a list containing three sublists, each of which consists of a sample of trapezoidal fuzzy numbers. The data corresponds to the \emph{quality} of the three main species of trees in Asturias, namely birch (\emph{Betula celtiberica}), sessile oak (\emph{Quercus petraea}) and rowan (\emph{Sorbus aucuparia}). Within a study about the progress of reforestation in a given area of Asturias (Spain) the INDUROT institute (University of Oviedo) has collected a sample of n1=133 birches, n2=109 sessile oaks and
n3=37 rowans. Each tree was assigned a trapezoidal fuzzy number that models the experts subjective judgements/perceptions of the tree quality on a scale from 0 to 5 (0 meaning very bad quality to 5 meaning very good quality). Thereby the 1-cut is the interval in which the expert thinks the quality is contained and the support (0-cut) is the interval in which the expert is absolutely sure the quality is contained.
}
\usage{data(Trees)}
\format{
 A list with three sublist, each of which contains trapezoidal fuzzy numbers.
}
\details{
See Reference
}
\source{
}
\references{
 [1] Colubi, A.: \emph{Statistical inference about the means of fuzzy random variables:
      Applications to the analysis of fuzzy- and real-valued data}, Fuzzy Sets and Systems,
      160(3), pp. 344-356 (2009)
}
\examples{
data(Trees)
M<-Mmean(Trees[[1]],1)
head(M)
M<-Mmean(Trees[[1]],1)
head(M)
Mmean(Trees[[1]],1)
head(M)
}
\keyword{datasets}
\keyword{list}
