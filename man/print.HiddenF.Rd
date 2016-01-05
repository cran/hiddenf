\name{print.HiddenF}
\alias{print.HiddenF}
\alias{print}
\title{
Printing hiddenf objects
}
\description{`print' method for class `HiddenF'
}
\usage{
\method{print}{HiddenF}(x, method = "ACMIF", ...)
}
\arguments{
  \item{x}{An object of class `HiddenF' }
  \item{method}{The name of the test for interaction.  Could be "ACMIF","TUKEY","MANDEL","KKSA", or "MALIK"}
  \item{\dots}{further arguments}
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

Malik, WA, Mohring, J and Piepho, H. (2015) A clustering-based test for non-additivity in an unreplicated two-way layout, Communications in Statistics-Simulation and Computation.  }
\author{
Jason A. Osborne, Christopher T. Franck and Bongseog Choi }
\seealso{ HiddenF }
\examples{
data(cnv1.mtx)
cnv1.out <- HiddenF(cnv1.mtx)
print(cnv1.out)
} 
\keyword{non-additivity} 
