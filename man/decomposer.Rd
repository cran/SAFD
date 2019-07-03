\name{decomposer}
\alias{decomposer}
\title{
Decomposer
}
\description{
Given a dataframe \code{X} the function first calls \code{checking} in order to test if \code{X} is in the desired format. If yes, the dataframe \code{X} (polygonal fuzzy number) is expressed as a dataframe with (nrow(X)+1) rows as described in the paper [1] below, if no, NULL is returned. The main aim of \code{decomposer} is to provide the simulator-function called \code{generator} with the correct input.
}
\usage{
decomposer(X)
}
\arguments{
  \item{X}{
  ...dataframe, if checking(X)=1 the decomposed version of \code{X} is returned.
}
}
\details{
See examples
}
\value{
In case checking(X)=1 \code{decomposer} returns a dataframe with (nrow(X+1))-rows (see [1]), otherwise NA is returned.
}
\references{
[1] Gonzalez-Rodriguez, G., Colubi, A., Trutschnig, W.: 
     \emph{Simulation of fuzzy random variables},  Inf.Sci., 179(5), pp. 642-653 (2009)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}

\seealso{
 See also \code{\link{checking}}, \code{\link{generator}}
}
\examples{
#Example:
 data(XX)
 A<-decomposer(XX[[2]])
 A<-decomposer(XX[[1]])
head(A)
}

\keyword{ nonparametric }
\keyword{ datagen }

