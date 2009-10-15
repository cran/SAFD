\name{SAFD-package}
\Rdversion{0.02}
\alias{SAFD-package}
\alias{SAFD}
\docType{package}
\title{
Statistical Analysis of Fuzzy Data
}
\description{
The aim of the package is to provide some basic functions for doing statistics with 
one-dimensional Fuzzy Data (in the form of polygonal fuzzy numbers). 
}
\details{
\tabular{ll}{
Package: \tab SAFD\cr
Type: \tab Package\cr
Version: \tab 0.02\cr
Date: \tab 2009-10-15\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
}
The package allows to work with polygonal fuzzy numbers, being represented as data frames with columns x and alpha (equidistant alpha levels in [0,1]). SAFD contains functions for the basic operations on the class of fuzzy numbers (sum, scalar product, mean, Hukuhara difference) as well as for calculating (Bertoluzza-) distance, sample variance, sample covariance, sample correlation, and the Dempster-Shafer (levelwise) histogram. Moreover a function to simulate fuzzy random variables, bootstrap tests for the equality of means, and a function to do linear regression given trapezoidal fuzzy data is included.
}
\author{
Wolfgang Trutschnig <wolfgang.trutschnig@softcomputing.es>, Asun Lubiano <lubiano@uniovi.es>

Maintainer: Wolfgang Trutschnig <wolfgang.trutschnig@softcomputing.es>, Asun Lubiano <lubiano@uniovi.es>
}
\references{
 [1] Bertoluzza, C., Corral, N., Salas, A.: \emph{On a new class of distances between fuzzy numbers}, Mathware Soft Comput., 2, pp:71-84 (1995)\cr
 [2] Colubi, A.: \emph{Statistical inference about the means of fuzzy random variables: Applications to the analysis of fuzzy- and real-valued data}, Fuzzy Sets and Systems, 160(3), pp. 344-356 (2009) \cr
 [3] Gil, M.A., Lopez, M.T., Lubiano, M.A., Montenegro, M.: \emph{Regression and correlation analyses of a linear relation between random intervals}, Test, 10(1), pp. 183-201 (2001) \cr
 [4] Gil, M.A.; Montenegro, M.; Gonzalez-Rodriguez, G.; Colubi, A.; Casals, R.: \emph{Bootstrap approach to the multi-sample test of means with imprecise data}, Computational Statistics and Data Analysis, 51(1), pp. 148-162  (2006) \cr
 [5] Gonzalez-Rodriguez, G.; Blanco, A.; Colubi, A.; Lubiano, M.A.: \emph{Estimation of a simple linear regression model for fuzzy random variables}, Fuzzy Sets and Systems, 160(3), pp. 357-370 (2009) \cr
 [6] Gonzalez-Rodriguez, G., Colubi, A., Trutschnig, W.: \emph{Simulation of fuzzy random variables},  Information Sciences, 179(5), pp. 642-653 (2009) \cr
 [7] Montenegro, M., Colubi, A., Casals, M.R., Gil, M.A.: \emph{Asymptotic and bootstrap techniques for testing the expected value of a fuzzy random variable}, Metrika, 59, pp. 31-49  (2004)\cr
 [8] Montenegro, M., Casals, M.R., Lubiano, M.A., Gil, M.A.: \emph{Two-sample hypothesis tests of means of a fuzzy random variable}, Information Sciences, Vol. 133(1-2), pp. 89-100 (2001)\cr
 [9] Trutschnig, W., \emph{A strong consistency result for fuzzy relative frequencies interpreted as estimator for the fuzzy-valued probability}, Fuzzy Sets and Systems, Vol. 159, nr 3, pp. 259-269 (2008) \cr
 [10] Trutschnig, W., Gonzalez-Rodriguez, G., Colubi, A.; Gil, M.A.: \emph{A new family of metrics for compact, convex (fuzzy) sets based on a generalized concept of mid and spread},  Information Sciences, 179(23), pp. 3964-3972 (2009) \cr
 [11] Viertl, R., Hareter, D.: \emph{Beschreibung und Analyse unscharfer Information: Statistische Methoden fuer unscharfe Daten}, Springer Wien New York, 2006 
 
}

\keyword{ package }
\seealso{
 \url{http://bellman.ciencias.uniovi.es/SMIRE/}
}
\examples{
}