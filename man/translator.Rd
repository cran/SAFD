\name{translator}
\alias{translator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Translator
}
\description{
The function first calls \code{checking2} in order to check if the input data \code{X} defines a polygonal fuzzy number. If all conditions are satisfied the function transforms the input \code{X} into a dataframe with the chosen number \code{nl} of levels (default is \code{nl}=101) by doing linear interpolation, and returns this dataframe.
}
\usage{
translator(X, nl = 101, pic = 0)
}
\arguments{
  \item{X}{
  ...dataframe for which \code{checking2} yields 1
}
  \item{nl}{
...number of levels of the output dataframe (fuzzy number), by default 101, must be at least 2.
}
  \item{pic}{
...numeric, if \code{pic}=1 then the sample mean of \code{XX} is printed. By default \code{pic}=0.
}
}
\details{
See examples
}
\value{
In case checking2(X)=1 \code{translator} returns a dataframe (fuzzy number) with \code{nl} number of alpha-levels, otherwise the input is returned unchanged.
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{checking2}}, \code{\link{checking}}
}
\examples{
#Example 1:
X<-data.frame(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,1,1,0))
E<-translator(X,3)
head(E)


#Example 2:
data(XX)
E<-translator(XX[[3]],11, pic=1)


## The function is currently defined as
function (X, nl = 101, pic = 0) 
{
    ok <- checking2(X)
    if (nl <= 1) {
        print("Minimum number of output levels nl is 2")
    }
    if (nl > 1 & ok == 1) {
        levels <- seq(0, 1, length = nl)
        A <- subset(X, X$alpha == 1)
        cut1 <- min(as.numeric(row.names(A)))
        cut2 <- max(as.numeric(row.names(A)))
        Left <- X[1:cut1, ]
        Right <- X[cut2:nrow(X), ]
        L1 <- approx(Left$alpha, Left$x, method = "linear", rule = 2, 
            n = nl)
        L2 <- cbind(L1$y, L1$x)
        L3 <- L2
        L4 <- data.frame(x = L3[, 1], alpha = L3[, 2])
        R1 <- approx(Right$alpha, Right$x, method = "linear", 
            rule = 2, n = nl)
        R2 <- cbind(R1$y, R1$x)
        R3 <- R2[nrow(R2):1, ]
        R4 <- data.frame(x = R3[, 1], alpha = R3[, 2])
        E <- rbind(L4, R4)
        if (pic == 1) {
            limx <- c(min(c(X$x, E$x)) - 0.25, max(c(X$x, E$x)) + 
                0.25)
            plot(X, type = "l", xlim = limx, xlab = NA, ylab = expression(alpha), 
                col = "gray", lwd = 1.5, cex.main = 1)
            titletxt <- substitute("Fuzzy number (in gray) and their " * 
                nl * "-Translator (in black)", list(nl = as.character(nl)))
            title(main = titletxt, cex.main = 1)
            lines(E, type = "b", lwd = 2, pch = 21)
        }
        invisible(E)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ attribute }
\keyword{ classes }% __ONLY ONE__ keyword per line
