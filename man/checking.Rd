\name{checking}
\alias{checking}
\title{
Checking correct data format 
}
\description{
The function checks if the input data is of the correct form of a polygonal fuzzy number, i.e. a dataframe with the columns "x" and "alpha" fulfilling the following conditions: (1) alpha-values have to be in [0,1] with the minimum alpha-level being 0 and maximum being 1, (2) the x-values have to be non-missing and non-decreasing, (3) the alpha-levels have to increase from 0 to 1 and afterwards decrease from 1 to 0 in the same way (i.e. the alpha-column consists of an increasing vector from 0 to 1 plus the same vector in decreasing order). As a consequence the dataframe always has an even number of rows, see examples. The function is used internally in almost all the other functions to do a preliminary checking if the input data is of the correct form.
}
\usage{
checking(X, com = 1)
}
\arguments{
  \item{X}{
  ...can be any data frame.
}
  \item{com}{
 ...numeric, if \code{com}=1 then, in case of an error, a comment is printed. By default \code{com}=1.
}
}
\details{
 See examples
}
\value{
The function returns the value 1 if the input fulfills all conditions, if not, 0 is returned. 
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
 See Also as \code{\link{checking2}}, \code{\link{translator}}
}
\examples{
#Example 1:
data(XX)
a<-checking(XX[[1]],1)
a

#Example 2:
X<-data.frame(y=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,0.9,0.9,0))
a<-checking(X)
a

#Example 3:
X<-data.frame(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,0.9,0.9,0))
a<-checking(X)
a

#Example 4:
X<-data.frame(x=c(-2,-0.75,-0.25,-0.5,1),alpha=c(0,0.6,1,1,0))
a<-checking(X)
a

#Example 5:
X<-data.frame(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0.3,0,1,0,0.3))
a<-checking(X)
a

#Example 6:
Y<-data.frame(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.3,1,0,0.3))
a<-checking(Y)
a

#Example 7:
Z<-data.frame(x=c(-2,-0.75,-0.25,0.5,1),alpha=c(0,0.6,1,1,0))
a<-checking(Z)
a

#Example 8:
U<-data.frame(x=c(-1,0,1),alpha=c(0,1,0))
a<-checking(U,)
a
## The function is currently defined as
function (X, com = 1) 
{
    temp <- rep(1, 7)
    if (names(X)[1] != c("x") | names(X)[2] != c("alpha")) {
        temp[1] <- 0
      print(paste("input has to be a data frame with column names x and alpha"))
        invisible(c(0))
    }
    if (temp[1] == 1) {
        if (min(X$alpha) != 0 | max(X$alpha) != 1) {
            temp[2] <- 0
            if (com == 1) {
                print(paste("input defines no polygonal fuzzy number"))
                print(paste("max alpha-value has to be 1, min 0"))
            }
            invisible(c(0))
        }
    }
    if (min(temp[1:2]) == 1) {
        if (all(complete.cases(X$x)) == FALSE) {
            temp[3] <- 0
            if (com == 1) {
                print(paste("input defines missing value", sep = ""))
            }
            invisible(c(0))
        }
    }
    if (min(temp[1:3]) == 1) {
        dat <- c(X$x[1], X$x)
        shifted <- c(X$x, X$x[nrow(X)])
        if (min(shifted - dat) < -.Machine$double.eps^0.5) {
            temp[4] <- 0
            if (com == 1) {
                print(paste("input defines no polygonal fuzzy number"))
                print(paste("x values must be non-decreasing", 
                  sep = ""))
            }
            invisible(c(0))
        }
    }
    if (min(temp[1:4]) == 1) {
        A <- subset(X, X$alpha == 1)
        cut1 <- min(as.numeric(row.names(A)))
        cut2 <- max(as.numeric(row.names(A)))
        fromto <- seq(cut1, cut2, by = 1)
        difference <- setdiff(fromto, as.numeric(row.names(A)))
        Left <- X[1:cut1, ]
        Right <- X[cut2:nrow(X), ]
        dat <- c(Left$alpha[1], Left$alpha)
        shifted <- c(Left$alpha, Left$alpha[nrow(Left)])
        if (min(shifted - dat) < 0 | length(difference) > 0) {
            temp[5] <- 0
            if (com == 1) {
                print(paste("input defines no polygonal fuzzy number"))
                print(paste("alpha-levels must increase from 0 to 1"))
                print(paste("and decrease from 1 to 0"))
            }
            invisible(c(0))
        }
    }
    if (min(temp[1:5]) == 1) {
        dat <- c(Right$alpha[1], Right$alpha)
        shifted <- c(Right$alpha, Right$alpha[nrow(Right)])
        if (min(dat - shifted) < 0) {
            temp[6] <- 0
            if (com == 1) {
                print(paste("input defines no polygonal fuzzy number"))
                print(paste("alpha-levels must increase from 0 to 1"))
                print(paste("and decrease from 1 to 0"))
            }
            invisible(c(0))
        }
    }
    if (min(temp[1:6]) == 1) {
        nl <- nrow(X)/2
        if (nl != trunc(nl, 0) | max(abs(X$alpha[1:nl] - X$alpha[(2 * 
          nl):(nl + 1)])) > 0) {
          temp[7] <- 0
          if (com == 1) {
            print(paste("input fuzzy number is not of the desired form"))
            print(paste("(even number nr of rows"))
            print(paste("and X$alpha[1:(nr/2)]=X$alpha[nr:(nr/2+1)])"))
            print(paste("use translator function to convert in the correct form"))
          }
            invisible(c(0))
        }
    }
    invisible(min(temp))
  }
}
\keyword{ attribute }
\keyword{ classes }

