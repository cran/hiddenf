\name{MalikPvalue}
\alias{MalikPvalue}
\title{Malik's test for non-additivity
}
\description{Computes the p-value from the clustering-based test for 
non-additivity developed in Malik, et al. (2015).
}
\usage{
MalikPvalue(hfobj, N=1000)
}
\arguments{
  \item{hfobj}{An object of class \code{hiddenf} created by the \code{hiddenf} function}
  \item{N}{The number of Monte Carlo datasets used to determine critical thresholds for Malik's test statistic.  Default value is N=1000.
}
}
\value{A Monte Carlo estimate of the p-value from the Malik et al (2015) test of non-additivity.  The standard error of this estimate is inversely proportional to the square root of N.}
\references{
Malik, WA, Mohring, J and Piepho, H. (2014) A clustering-based test for non-additivity in an unreplicated two-way layout, Communications in Statistics-Simulation and Computation.
}
\author{Jason A. Osborne, Christopher T. Franck and Bongseog Choi 
}
\seealso{hiddenf, additivityPvalues
}
\examples{
library(hiddenf)
data(cjejuni.mtx)
cjejuni.out <- hiddenf(cjejuni.mtx)
cjejuni.MalikPvalue <- MalikPvalue(cjejuni.out)
}
\keyword{non-additivity }
