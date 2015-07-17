\name{generator}
\alias{generator}
\title{
 Simulation of fuzzy random variables (FRV)
}
\description{
The second procedure described in [1] is implemented. Given an input dataframe \code{V} in the correct format (tested by \code{checking}), which will be the expectation of the simulated FRV first \code{decomposer(V)} is called. Loosely speaking, the dataframe \code{Y} returned by \code{decomposer}, which contains the "coordinates" of \code{V} with respect to a certain "basis" (see [1]), is perturbated stochastically in order to generate a new polygonal fuzzy number. The distributions used for these perturbations can be selected in the call of the function, however, in this version only a few choices are possible: (1) The perturbation of the centre of the 1-cut \code{pertV} has to be of the form \code{norm(0,sigma)} or \code{unif(-a,a)}, sigma,a>0. (2) The perturbation of the left part of the fuzzy set \code{pertL} has to be of the form \code{chisq(1)}, \code{exp(1)} or \code{lnorm(a,b)} with expectation=1. (3) The perturbation of the right part of the fuzzy set \code{pertR} has to be of the same form as that for the left part.
}
\usage{
generator(V, pertV = list(dist = "norm", par = c(0, 1)), 
             pertL = list(dist = "chisq", par = c(1)), 
             pertR = list(dist = "chisq", par = c(1)))
}
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
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{decomposer}}
}

\examples{
#Example:
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

## The function is currently defined as
function (V, pertV = list(dist = "norm", par = c(0, 1)), 
              pertL = list(dist = "chisq", par = c(1)), 
              pertR = list(dist = "chisq", par = c(1))) 
{
    nl <- nrow(V)/2
    B <- decomposer(V)
    if (nrow(B) > 1) {
        vc <- B$coor[nl + 1]
        cl <- B$coor[1:nl]
        cr <- B$coor[(nl + 2):(2 * nl + 1)]
        if (is.list(pertV) == FALSE | is.list(pertL) == FALSE | 
            is.list(pertR) == FALSE) {
            print("pertV, pertL and pertR have to be lists")
        }
        if (is.list(pertV) == TRUE & is.list(pertL) == TRUE & 
            is.list(pertR) == TRUE) {
            allowed <- c("unif", "norm", "chisq", "lnorm", "exp")
            if (!pertV$dist \%in\% allowed[1:2]) {
                print(paste("chosen distributions for the perturbations"))
                print(paste("of the centre of the 1-cut"))
                print(paste("must be normal or uniform"))
            }
            if (!pertL$dist \%in\% allowed[3:5] | !pertR$dist \%in\% 
                allowed[3:5]) {
                print(paste("chosen distributions for"))
                print(paste("the (left/right) perturbations"))
                print(paste("must be chisquare, lognormal or exponential"))
            }
            if (pertV$dist == "norm") {
                if (pertV$par[1] != 0) {
                  print(paste("expectation of perturbation"))
                  print(paste("of the mid of the 1-cut"))
                  print(paste("must have expectation 0"))
                }
                VC <- rnorm(1, 0, pertV$par[2]) + vc
            }
            if (pertV$dist == "unif") {
                if ((pertV$par[1] + pertV$par[2]) != 0) {
                  print(paste("expectation of perturbation"))
                  print(paste("of the mid of the 1-cut"))
                  print(paste("must have expectation 0"))
                }
                VC <- runif(1, pertV$par[1], pertV$par[2]) + 
                  vc
            }
            if (pertL$dist == "chisq") {
                if (pertL$par[1] != 1) {
                  print(paste("expectation of (left) perturbation"))
                  print(paste("must have expectation 1 and be nonnegative"))
                }
                perl <- rchisq(nl, 1)
            }
            if (pertL$dist == "lnorm") {
                if (exp(pertL$par[1] + pertL$par[2]^2/2) != 1) {
                  print(paste("expectation of (left) perturbation"))
                  print(paste("must have expectation 1 and be nonnegative"))
                }
                perl <- rlnorm(nl, pertL$par[1], pertL$par[2])
            }
            if (pertL$dist == "exp") {
                if (pertL$par[1] != 1) {
                  print(paste("expectation of (left) perturbation"))
                  print(paste("must have expectation 1 and be nonnegative"))
                }
                perl <- rexp(nl, pertL$par[1])
            }
            if (pertR$dist == "chisq") {
                if (pertR$par[1] != 1) {
                  print(paste("expectation of (right) perturbation"))
                  print(paste("must have expectation 1 and be nonnegative"))
                }
                perr <- rchisq(nl, 1)
            }
            if (pertR$dist == "lnorm") {
                if (exp(pertR$par[1] + pertR$par[2]^2/2) != 1) {
                  print(paste("expectation of (right) perturbation"))
                  print(paste("must have expectation 1 and be nonnegative"))
                }
                perr <- rlnorm(nl, pertR$par[1], pertR$par[2])
            }
            if (pertR$dist == "exp") {
                if (pertR$par[1] != 1) {
                  print(paste("expectation of (right) perturbation"))
                  print(paste("must have expectation 1 and be nonnegative"))
                }
                perr <- rexp(nl, pertR$par[1])
            }
            CL <- cl * perl
            CR <- cr * perr
            XL <- (VC - cumsum(CL[nl:1]))[nl:1]
            XR <- (VC + cumsum(CR))
            X <- data.frame(x = c(XL, XR), alpha = V$alpha)
            invisible(X)
        }
    }
  }
}
\keyword{ nonparametric }
\keyword{ datagen }
