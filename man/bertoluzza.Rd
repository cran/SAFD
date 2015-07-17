\name{bertoluzza}
\alias{bertoluzza}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bertoluzza distance
}
\description{
Given two polygonal fuzzy numbers \code{X}, \code{Y} in the correct format (testing by \code{checking}) the function calculates the Bertoluzza distance of \code{X}, \code{Y}. The parameter \code{theta} (being the weight of the spread) has to fulfill \code{theta}>0, by default \code{theta}=1/3 (which corresponds to the Lebesgue measure as weighting measure on [0,1]). For detailed explanation see the papers [1] and [2] below.
}
\usage{
bertoluzza(X, Y, theta = 1/3, pic = 0)
}
\arguments{
  \item{X}{
...dataframe (polygonal fuzzy number)
}
  \item{Y}{
...dataframe (polygonal fuzzy number)
}
  \item{theta}{
...numeric and >0
}
  \item{pic}{
...numeric, if \code{pic}==1 \code{X} and \code{Y} are plotted, by default no plot is produced. 
}
}
\details{
See examples
}
\value{
...in case \code{X} and \code{Y} fulfill the conditions tested implicitly by \code{checking} the code returns the Bertoluzza distance, otherwise NA is returned.
}
\references{
[1] Trutschnig, W., Gonzalez-Rodriguez, G., Colubi, A., Gil, M.A.: \emph{A new family of metrics for compact, convex (fuzzy) sets based on a generalized concept of mid and spread},  Information Sciences, 179(23), pp. 3964-3972 (2009) 
     
[2] Bertoluzza, C., Corral, N., Salas, A.: \emph{On a new class of distances between fuzzy numbers}, Mathware Soft Comput., 2, pp:71-84 (1995)
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{checking}}, \code{\link{Mmean}}, \code{\link{Bvar}}, \code{\link{Bcov}}
}
\examples{
#Example 1:
data(XX)
X<-translator(XX[[1]],50)
Y<-translator(XX[[2]],50)
Z<-translator(XX[[3]],50)
ZZ<-list(X,Y,Z)
b<-bertoluzza(X,Y,1/3,1)
b

#Example 2: example (SLLN for the FRV)
V<-translator(XX[[3]],100)
YY<-vector("list",length=50)
  for(i in 1:50){
   YY[[i]]<-generator(V,,,)
   }
M<-Mmean(YY)
head(M)
b<-bertoluzza(M,V,1/3,1) 
b

#Example 3:
V<-translator(XX[[3]],100)
YY<-vector("list",length=1000)
  for(i in 1:1000){
   YY[[i]]<-generator(V,,,)
   }
M<-Mmean(YY)
head(M)
b<-bertoluzza(M,V,1/3,1)  
b

#Example 4:
X<-data.frame(x=c(0,1,1,2),alpha=c(0,1,1,0))
Y<-data.frame(x=c(0,1,2),alpha=c(0,1,0))
b<-bertoluzza(X,Y,1/3,1)
b

#Example 5:
data(Trees)
X<-Mmean(Trees[[1]])
Y<-Mmean(Trees[[2]])
Z<-Mmean(Trees[[3]])
b1<-bertoluzza(X,Y,1)
b1
b2<-bertoluzza(X,Z,1)
b2
b3<-bertoluzza(Y,Z,1)
b3


## The function is currently defined as
function (X, Y, theta = 1/3, pic = 0) 
{
    temp_sum <- Msum(list(X, Y))
    if (is.null(temp_sum) == 0) {
        nl <- nrow(X)/2
        z <- X$x - Y$x
        integrand1 <- z[1:nl] + z[(2 * nl):(nl + 1)]
        integrand2 <- z[1:nl] - z[(2 * nl):(nl + 1)]
        l2dist2 <- function(x) {
            k <- length(x) - 1
            delta <- 1/k
            y <- x[1:k] + x[2:(k + 1)]
            values <- x[1:k]^2 + x[2:(k + 1)]^2 + y^2
            integral <- sum(values) * delta/6
            invisible(integral)
        }
        d1 <- (l2dist2(integrand1) + theta * l2dist2(integrand2)) * 
            0.25
        distance <- (d1)^0.5
        dis <- round(distance, 2)
        th <- round(theta, 2)
        if (pic == 1) {
            limx <- c(min(c(X$x, Y$x)) - 0.25, max(c(X$x, Y$x)) + 
                0.25)
            plot(X, type = "l", xlim = limx, lwd = 2, xlab = NA, 
                ylab = expression(alpha), cex.main = 1)
            titletxt <- substitute(D[th] * "=" * dis, list(th = as.character(th), 
                dis = dis))
            title(main = titletxt, cex.main = 1)
            lines(Y, type = "l", lwd = 2, col = "black")
        }
        invisible(distance)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
\keyword{ manip }% __ONLY ONE__ keyword per line
