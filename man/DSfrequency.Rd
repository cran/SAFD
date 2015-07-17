\name{DSfrequency}
\alias{DSfrequency}
\title{
Levelwise Dempster-Shafer frequency
}
\description{
Given a sample \code{XX} of polygonal fuzzy numbers and an interval \code{IV} the levelwise Dempster-Shafer frequency of the interval is calculated, i.e. for the chosen number \code{nl} of equidistant alpha-cuts it is checked how many of the elements of the sample have an alpha-cut that is contained in the interval (lower frequency) and how many have an alpha-cut hitting the interval (upper frequency). These family of intervals is afterwards aggregated to another polygonal fuzzy number with the corresponding number of alpha-cuts, which is returned. For details see [1] below. Preliminary the input data is tested for the correct format using the \code{checking} function.
}
\usage{
DSfrequency(XX, IV = c(0, 1), pic = 1, nl = 101)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers (the functions implicitly checks the conditions) 
}
  \item{IV}{
...numeric vector of length two, by default \code{IV}=c(0,1)
}
  \item{pic}{
...numeric, in case \code{pic}=1 the frequency is plotted, otherwise no plot is produced
}
  \item{nl}{
...number of equidistant alpha-levels, by default \code{nl}=101
}
}
\details{
See examples
}
\value{
Given correct input data, the function returns the levelwise Dempster-Shafer frequency of the chosen interval (again in the correct form of a polygonal fuzzy number).
}
\references{
[1] Trutschnig, W., \emph{A strong consistency result for fuzzy relative frequencies interpreted as estimator for the fuzzy-valued probability}, Fuzzy Sets and Systems, Vol. 159, nr 3, pp. 259-269 (2008) \cr
[2] Viertl, R., Hareter, D.: \emph{Beschreibung und Analyse unscharfer Information: Statistische Methoden fuer unscharfe Daten}, Springer Wien New York, 2006 
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{checking2}}, \code{\link{translator}}, \code{\link{DShistogram}}
}
\examples{
# Example:
data(XX)
V<-XX[[1]]
SS<-vector("list",length=500)
for (j in 1:500){
 SS[[j]]<-generator(V,)
 }
A<-DSfrequency(SS,c(1,3),1,100)
head(A)

## The function is currently defined as
function (XX, IV = c(0, 1), pic = 1, nl = 101) 
{
    k <- length(XX)
    temp <- rep(0, k)
    for (i in 1:k) {
        temp[i] <- checking2(XX[[i]], 0)
    }
    if (min(temp) == 0) {
        print("One or more elements of the input data defines no polygonal fuzzy number")
    }
    if (min(temp) == 1) {
        X <- translator(XX[[1]], nl)
        YY <- vector("list", length = k)
        YY[[1]] <- X
        hitting <- rep(0, nl)
        contained <- rep(0, nl)
        if (X$x[1] <= IV[2] & X$x[2 * nl] >= IV[1]) {
            hitting <- ifelse(X$x[1:nl] > IV[2] | X$x[(2 * nl):(nl + 
                1)] < IV[1], 0, 1)
            contained <- ifelse(X$x[1:nl] >= IV[1] & X$x[(2 * 
                nl):(nl + 1)] < IV[2], 1, 0)
        }
        if (k == 1) {
            f <- data.frame(x = c(contained, hitting), alpha = X$alpha)
            if (pic == 1) {
                plot(YY[[1]], type = "l", lwd = 0.1, xlab = "x", 
                  ylab = expression(alpha), cex.main = 1, col = "gray50", 
                  main = paste("Sample and chosen interval", 
                    sep = ""))
                Left <- data.frame(x = rep(IV[1], 2), alpha = c(-0.05, 
                  1.05))
                Right <- data.frame(x = rep(IV[2], 2), alpha = c(-0.05, 
                  1.05))
                lines(Left, type = "l", col = "red", lwd = 2)
                lines(Right, type = "l", col = "red", lwd = 2)
                dev.new()
                plot(f, type = "l", xlim = c(0, 1), ylim = c(0, 
                  1), main = paste("Levelwise Dempster Shafer frequency of the interval [", 
                  IV[1], ",", IV[2], "]", sep = ""), cex.main = 1, 
                  xlab = "x", ylab = expression(alpha))
                lines(f, type = "p", cex = 0.2)
            }
            invisible(f)
        }
        if (k > 1) {
            for (i in 2:k) {
                X <- translator(XX[[i]], nl)
                YY[[i]] <- X
                hitting_dazu <- rep(0, nl)
                contained_dazu <- rep(0, nl)
                if (X$x[1] <= IV[2] & X$x[2 * nl] >= IV[1]) {
                  hitting_dazu <- ifelse(X$x[1:nl] > IV[2] | 
                    X$x[(2 * nl):(nl + 1)] < IV[1], 0, 1)
                  contained_dazu <- ifelse(X$x[1:nl] >= IV[1] & 
                    X$x[(2 * nl):(nl + 1)] < IV[2], 1, 0)
                }
                hitting <- hitting + hitting_dazu
                contained <- contained + contained_dazu
            }
            hitting <- 1/k * hitting
            contained <- 1/k * contained
            f <- data.frame(x = c(contained, hitting[nl:1]), 
                alpha = YY[[1]]$alpha)
            if (pic == 1) {
                lower <- rep(0, k)
                upper <- lower
                for (j in 1:k) {
                  lower[j] <- min(YY[[j]])
                  upper[j] <- max(YY[[j]])
                }
                limx <- c(min(lower), max(upper))
                plot(YY[[1]], type = "l", xlim = limx, lwd = 0.1, 
                  xlab = "x", ylab = expression(alpha), cex.main = 1, 
                  col = "gray50", main = paste("Sample and chosen interval", 
                    sep = ""))
                for (j in 2:k) {
                  lines(YY[[j]], type = "l", lwd = 0.1, col = "gray50")
                }
                Left <- data.frame(x = rep(IV[1], 2), alpha = c(-0.05, 
                  1.05))
                Right <- data.frame(x = rep(IV[2], 2), alpha = c(-0.05, 
                  1.05))
                lines(Left, type = "l", col = "red", lwd = 2)
                lines(Right, type = "l", col = "red", lwd = 2)
                dev.new()
                plot(f, type = "l", xlim = c(0, 1), ylim = c(0, 
                  1), main = paste("Levelwise Dempster Shafer frequency of the interval [", 
                  IV[1], ",", IV[2], "]", sep = ""), cex.main = 1, 
                  xlab = "x", ylab = expression(alpha))
                lines(f, type = "p", cex = 0.2)
            }
            invisible(f)
        }
    }
  }
}
\keyword{ manip }
\keyword{ datagen }

