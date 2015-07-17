\name{Bvar}
\alias{Bvar}
\title{
(Sample) Variance
}
\description{
The sample variance of a sample of polygonal fuzzy numbers with respect to the Bertoluzza distance is calculated. Given a list \code{XX} of polygonal fuzzy numbers the function first checks if each element of the list has the correct form and if the alpha-levels of all elements in the list coincide. If these conditions are fulfilled the Bertoluzza sample variance will be returned (i.e. the average Bertoluzza distance of the elements of \code{XX} to its mean). If not the \code{translator} function can be used to transform the elements of the list in the correct format. For details see [1] from below, and replace the kernel K with the expression induced by the Bertoluzza metric. The parameter \code{theta} has to fulfill \code{theta}>0.
}
\usage{
Bvar(XX, theta = 1/3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...should be a list of polygonal fuzzy numbers (the functions implicitly checks the conditions) verifying the above mentioned conditions 
}
  \item{theta}{
...numeric and >0, see \code{bertoluzza}
}
}
\details{
See examples
}
\value{
Given input \code{XX} in the correct format the function returns the Bertoluzza variance of the sample \code{XX}.
}
\references{
[1] Gonzalez-Rodriguez, G.; Blanco, A.; Colubi, A.; Lubiano, M.A.: \emph{Estimation of a simple linear regression model for fuzzy random variables}, Fuzzy Sets and Systems, 160(3), pp. 357-370 (2009)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{bertoluzza}}, \code{\link{Mmean}}, \code{\link{Bcov}}
}
\examples{
#Example 1:
data(XX)
X<-translator(XX[[1]],50)
Y<-translator(XX[[2]],50)
Z<-translator(XX[[3]],50)
YY<-list(X,Y,Z)
A<-Bvar(YY,1)
A

#Example 2:
data(XX)
v<-Bvar(list(XX[[1]],XX[[1]]),1/3)
v

#Example 3:
data(Trees)
Species1_Var<-Bvar(Trees$species1,1/3)
Species1_Var
Species2_Var<-Bvar(Trees$species2,1/3)
Species2_Var
Species3_Var<-Bvar(Trees$species3,1/3)
Species3_Var


#Example 4:
data(quality)
Soilquality_Var<-Bvar(quality$land,1)
Soilquality_Var
Treesquality_Var<-Bvar(quality$trees,1)
Treesquality_Var

## The function is currently defined as
function (XX, theta = 1/3) 
{
    sample_mean <- Mmean(XX, 0)
    if (is.null(sample_mean) == 0) {
        if (length(XX) == 1) {
            v <- 0
        }
        if (length(XX) >= 2) {
            k <- length(XX)
            temp <- rep(0, k)
            for (i in 1:k) {
                temp[i] <- (bertoluzza(XX[[i]], sample_mean, 
                  theta, 0))^2
            }
            v <- mean(temp)
        }
        invisible(v)
    }
  }
}
\keyword{ manip }
\keyword{ univar }
