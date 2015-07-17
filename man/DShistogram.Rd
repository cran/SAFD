\name{DShistogram}
\alias{DShistogram}
\title{
Levelwise Dempster-Shafer Histogram
}
\description{
Based on a sample \code{XX} of polygonal fuzzy numbers (tested by \code{checking}), a chosen interval \code{limx}, a chosen number \code{npart} of partitions elements and a chosen number \code{nl} of equidistant alpha-levels the levelwise Dempster-Shafer frequency for each partition element is calculated. If \code{pic}=TRUE then a 3d plot as well as an image-plot of the histogram is produced. In case of \code{pdf}=TRUE a pdf containing these plots is produced automatically. 
}
\usage{
DShistogram(XX, limx = NA, npart = 10, nl = 101, pic = TRUE, pdf = FALSE)
}
\arguments{
  \item{XX}{
...list of polygonal fuzzy numbers (the function implicitly checks the conditions) 
}
  \item{limx}{
...numeric vector of length two, by default limx=c(0,1), that determines the x-range for which the histogram is plotted 
}
  \item{npart}{
...integer, number of partitions elements
}
  \item{nl}{
...number of equidistant alpha-level, by default \code{nl}=101
}
  \item{pic}{
...if \code{pic}=1, a 3d- and an image-plot of the histogram is produced
}
  \item{pdf}{
...if \code{pdf}=1, a 3d- and an image-plot of the histogram is automatically exported as pdf, by default \code{pdf}=FALSE
}
}
\details{
See examples
}
\value{
If the input data is in the correct form the function returns a list with the following elements:
 \item{gridx}{x-grid for plotting the histogram}
 \item{gridy}{y-grid for plotting the histogram}
 \item{M}{values of the histogram as function on the grid}
 \item{breaks}{breaks of the histogram}
}
\references{
[1] Trutschnig, W., \emph{A strong consistency result for fuzzy relative frequencies interpreted as estimator for the fuzzy-valued probability}, Fuzzy Sets and Systems, Vol. 159, nr 3, pp. 259-269 (2008)\cr
[2] Viertl, R., Hareter, D.: \emph{Beschreibung und Analyse unscharfer Information: Statistische Methoden fuer unscharfe Daten}, Springer Wien New York, 2006 
}
\author{
Wolfgang Trutschnig <wolfgang@trutschnig.net>, Asun Lubiano <lubiano@uniovi.es>
}
\note{
In case you find (almost surely existing) bugs or have recommendations for improving the functions comments are welcome to the above mentioned mail addresses.
}
\seealso{
See Also as \code{\link{DSfrequency}}
}
\examples{
#Example 1: an example with a random variable with small spread of the expectation
#run for bigger sample size and finer partition
data(XX)
V<-translator(XX[[3]],30)
V2<-V
V2$x<-V$x/5
SS<-vector("list",length=100)
for (j in 1:100){
 SS[[j]]<-generator(V2,)
 }
A<-DShistogram(SS,c(-3,3),npart=6,nl=51)

#Example 2: run for bigger sample size and finer partition
data(XX)
V<-translator(XX[[3]],30)
V2<-V
V2$x<-V$x/10
pertV<-list(dist="unif",par=c(-2,2))
SS<-vector("list",length=100)
for (j in 1:100){
 SS[[j]]<-generator(V2,pertV,)
 }
A<-DShistogram(SS,,npart=5,nl=51)

#Example 3: takes some time but produces nice result
#data(XX)
#V<-translator(XX[[3]],30)
#V2<-V
#V2$x<-V$x/10
#pertV<-list(dist="unif",par=c(-2,2))
#pertL<-list(dist="lnorm",par=c(-2,2))
#SS<-vector("list",length=1000)
#for (j in 1:1000){
# SS[[j]]<-generator(V2,pertV,pertL,)
# }
#A<-DShistogram(SS,,npart=15,nl=51)


## The function is currently defined as
function (XX, limx = NA, npart = 10, nl = 101, pic = TRUE, pdf = FALSE) 
{
    if (length(limx) <= 1 | limx[2] <= limx[1]) {
        a <- XX[[1]]$x[1]
        b <- XX[[1]]$x[nrow(XX[[1]])]
        if (length(XX) > 1) {
            for (i in 2:length(XX)) {
                a <- min(a, XX[[i]]$x[1])
                b <- max(b, XX[[i]]$x[nrow(XX[[i]])])
            }
        }
        limx <- c(a, b)
    }
    k <- length(XX)
    if (k > 500) {
        ygrid <- seq(0, 1, length = 501)
    }
    if (k <= 500) {
        ygrid <- sort(union(seq(0, 1, length = (k + 1)), seq(0, 
            1, length = 101)))
    }
    breaks <- seq(limx[1], limx[2], length = npart + 1)
    FR <- vector("list", length = npart)
    FR2 <- vector("list", length = npart)
    for (i in 1:npart) {
        FR[[i]] <- DSfrequency(XX, breaks[i:(i + 1)], 0, nl)
        print(i)
        R <- FR[[i]][(nl + 1):(2 * nl), ]
        a <- approx(R$x, R$alpha, xout = ygrid, yleft = R$alpha[1], 
            yright = R$alpha[nl], method = "constant", f = 1, 
            ties = "max")
        L <- FR[[i]][1:nl, ]
        b <- approx(L$x, L$alpha, xout = ygrid, yleft = L$alpha[1], 
            yright = L$alpha[nl], method = "constant", f = 0, 
            ties = "max")
        value <- ifelse(a$y >= b$y, b$y, a$y)
        FR2[[i]] <- data.frame(x = ygrid, y = value)
    }
    grid1 <- breaks + (breaks[2] - breaks[1])/1000
    grid2 <- breaks - (breaks[2] - breaks[1])/1000
    grid3 <- c(grid1, grid2)
    grid3 <- sort(subset(grid3, grid3 >= min(breaks) & grid3 <= 
        max(breaks)))
    gridx <- grid3
    gridy <- ygrid
    M <- matrix(numeric(npart * length(gridy)), ncol = length(gridy))
    for (i in 1:npart) {
        M[i, ] <- FR2[[i]]$y
    }
    M2 <- M[rep(1:npart, rep(2, npart)), ]
    k <- length(XX)
    lower <- rep(0, k)
    upper <- lower
    for (j in 1:k) {
        lower[j] <- min(XX[[j]])
        upper[j] <- max(XX[[j]])
    }
    lim_temp <- c(min(lower), max(upper))
    if (pdf == TRUE) {
        pdf(file = "histo.pdf", width = 12, height = 8)
        color <- rainbow(100, start = 0.7, end = 0.17)
        zfacet <- M2[-1, -1] + M2[-1, -ncol(M2)] + M2[-nrow(M2), 
            -1] + M2[-nrow(M2), -ncol(M2)]
        facetcol <- cut(zfacet, 100)
        M <- M2
        colmax <- rep(0, trunc(length(gridy)/10))
        for (i in 1:trunc(length(gridy)/10)) {
            colmax[i] <- max(M[, 10 * i])
        }
        Cut <- data.frame(nr = seq(1, length(colmax), by = 1), 
            colmax = colmax)
        Cut <- subset(Cut, Cut$colmax > 0)
        cutindex <- min(round(10 * Cut$nr[nrow(Cut)] * 1.25, 
            0), length(gridy))
        ym <- min(gridy[10 * Cut$nr[nrow(Cut)]] * 1.25, 1)
        Mp <- M[, 1:cutindex]
        gridyp <- gridy[1:cutindex]
        persp(gridx, gridyp, Mp, xlab = "x", ylab = "upper/lower frequency", 
            zlab = expression(alpha), xlim = limx, main = paste("Histogram 3d", 
                sep = ""), cex.main = 1, theta = -45, phi = 35, 
            expand = 0.35, col = color[facetcol], shade = 0.25, 
            ticktype = "detailed", border = NA)
        persp(gridx, gridyp, Mp, xlab = "x", ylab = "upper/lower frequency", 
            zlab = expression(alpha), xlim = limx, main = paste("Histogram 3d", 
                sep = ""), cex.main = 1, theta = 45, phi = 35, 
            expand = 0.35, col = color[facetcol], shade = 0.25, 
            ticktype = "detailed", border = NA)
        image(gridx, gridyp, Mp, xlab = "x", ylab = "upper/lower frequency", 
            xlim = limx, col = rainbow(100, start = 0.7, end = 0.17), 
            cex.axis = 1, main = paste("Histogram level view", 
                "\n", "(black lines denote 1-cut, white lines 0.5-cut)", 
                sep = ""), cex.main = 1)
        contour(gridx, gridyp, Mp, xlab = "", ylab = "", xlim = limx, 
            lwd = c(1.5, 1.5), levels = seq(0.5, 1, by = 0.5), 
            add = TRUE, col = c("white", "black"), lty = c(1, 
                1), drawlabels = FALSE)
        dev.off()
    }
    if (pic == TRUE) {
        color <- rainbow(100, start = 0.7, end = 0.17)
        zfacet <- M2[-1, -1] + M2[-1, -ncol(M2)] + M2[-nrow(M2), 
            -1] + M2[-nrow(M2), -ncol(M2)]
        facetcol <- cut(zfacet, 100)
        M <- M2
        colmax <- rep(0, trunc(length(gridy)/10))
        for (i in 1:trunc(length(gridy)/10)) {
            colmax[i] <- max(M[, 10 * i])
        }
        Cut <- data.frame(nr = seq(1, length(colmax), by = 1), 
            colmax = colmax)
        Cut <- subset(Cut, Cut$colmax > 0)
        cutindex <- min(round(10 * Cut$nr[nrow(Cut)] * 1.25, 
            0), length(gridy))
        ym <- min(gridy[10 * Cut$nr[nrow(Cut)]] * 1.25, 1)
        Mp <- M[, 1:cutindex]
        gridyp <- gridy[1:cutindex]
        persp(gridx, gridyp, Mp, xlab = "x", ylab = "upper/lower frequency", 
            zlab = expression(alpha), xlim = limx, main = paste("Histogram 3d", 
                sep = ""), cex.main = 1, theta = -45, phi = 35, 
            expand = 0.35, col = color[facetcol], shade = 0.25, 
            ticktype = "detailed", border = NA)
        dev.new()
        image(gridx, gridyp, Mp, xlab = "x", ylab = "upper/lower frequency", 
            xlim = limx, col = rainbow(100, start = 0.7, end = 0.17), 
            cex.axis = 1, main = paste("Histogram level view", 
                "\n", "(black lines denote 1-cut, white lines 0.5-cut)", 
                sep = ""), cex.main = 1)
        contour(gridx, gridyp, Mp, xlab = "", ylab = "", xlim = limx, 
            lwd = c(1.5, 1.5), levels = seq(0.5, 1, by = 0.5), 
            add = TRUE, col = c("white", "black"), lty = c(1, 
                1), drawlabels = FALSE)
    }
    H <- list(gridx = gridx, gridy = gridy, M = M, breaks = breaks)
    invisible(H)
  }
}
\keyword{ dplot }
\keyword{ datagen }
