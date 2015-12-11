\name{Mmean}
\alias{Mmean}
\title{
Minkowski mean
}
\description{
Given a sample \code{XX} of polygonal fuzzy numbers the Minkowski-mean of the sample is calculated. The function fist calls \code{Msum} to check if \code{XX} has the correct format and, in case yes, \code{sc_mult} is used to calculate the Minkowski-mean of the fuzzy sample \code{XX}. If \code{pic}=1 then the sample and its mean are plotted, otherwise no plot is produced.
}
\usage{
Mmean(XX, pic = 0)
}
\arguments{
  \item{XX}{
...should be a list of polygonal fuzzy numbers (the functions implicitly checks the conditions) 
}
  \item{pic}{
...numeric, if \code{pic}=1 then the sample mean of \code{XX} is printed. By default \code{pic}=0.
 }
}
\details{
See examples
}
\value{
Given input \code{XX} in the correct format the function returns the Minkowski mean of the polygonal fuzzy numbers contained in the list \code{XX}. 
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{checking}}, \code{\link{translator}}, \code{\link{Msum}}, \code{\link{sc_mult}}, \code{\link{Bvar}} 
}
\examples{
#Example 1:
data(XX)
A<-Mmean(XX,1)

X<-translator(XX[[1]],50)
Y<-translator(XX[[2]],50)
Z<-translator(XX[[3]],50)
YY<-list(X,Y,Z)
A<-Mmean(YY,pic=1)


#Example 2:
data(Trees)
Species1_Mean<-Mmean(Trees[[1]],1)
Species1_Mean
Species2_Mean<-Mmean(Trees[[2]],1)
Species2_Mean
Species3_Mean<-Mmean(Trees[[3]],1)
Species3_Mean

#Example 3:
data(quality)
Soilquality_Mean<-Mmean(quality$land,1)
Soilquality_Mean
Treesquality_Mean<-Mmean(quality$trees,1)
Treesquality_Mean
}

\keyword{ univar }
\keyword{ manip }
