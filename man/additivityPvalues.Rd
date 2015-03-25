\name{additivityPvalues}
\alias{additivityPvalues}
\title{Non-additivity pvalues
}
\description{Reports p-values tests for non-additivity
developed by Tukey (1949), Mandel (1961), 
Kharrati-Kopaei and Sadooghi-Alvandi (2007), 
Franck, Nielsen and Osborne (2014) and Malik, Mohring and Piepho (2015).
}
\usage{
additivityPvalues(ymtx.out)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ymtx.out}{
An object of class \code{hiddenf} created by the \code{hiddenf} function.
}
}
\value{
A list with five component p-values.
}
\references{
Tukey, JW (1949).  One Degree of Freedom for Non-Additivity.
Biometrics, 5:232-242. 

Mandel J. (1961) Non-Additivity in Two-Way Analysis of Variance,
Journal of the American Statistical Association, 56:878-888 

Kharrati-Kopaei, M. and Sadooghi-Alvandi, SM. (2007).
A New Method for Testing Interaction in Unreplicated
Two-Way Analysis of Variance, Communications in Statistics - Theory
and Methods, 36:2787-2803 

Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.

Malik, WA, Mohring, J and Piepho, H. (2015) A clustering-based test for non-additivity in an unreplicated two-way layout, Communications in Statistics-Simulation and Computation.
}
\author{
Jason A. Osborne \email{jaosborn@ncsu.edu}, Christopher T. Franck and Bongseog Choi 
}
\examples{
library(hiddenf)
data(cjejuni.mtx)
cjejuni.out <- hiddenf(cjejuni.mtx)
pvalues <- additivityPvalues(cjejuni.out)
print(pvalues)
}
\keyword{non-additivity}
