\name{MandelPvalue}
\alias{MandelPvalue}
\title{Mandel's rows-linear test for non-additivity
}
\description{Computes the p-value from Mandel's rows-linear test for non-additivity.  (The columns-linear test may be conducted by first transposing the input matrix argument.)
}
\usage{
MandelPvalue(hfobj)
}
\arguments{
  \item{hfobj}{An object of class \code{hiddenf} created by the \code{hiddenf} function
}
}
\value{
A p-value from a test of the hypothesis of additivity, along with component sums of squares used to compute p-value.
}
\references{
Mandel J. (1961) Non-Additivity in Two-Way Analysis of Variance, Journal of the American Statistical Association, 56:878-888.
}
\author{
Jason A. Osborne and Christopher T.  Franck and  Bongseog Choi
}
\seealso{hiddenf, additivityPvalues
}
\examples{
\dontrun{
library(hiddenf)
data(cjejuni.mtx)
cjejuni.out <- hiddenf(cjejuni.mtx)
kksa.pvalue <- KKSAPvalue(cjejuni.out)
}}
\keyword{non-additivity }
