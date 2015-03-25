\name{Malikfunction}
\alias{Malikfunction}
\title{Function for Malik's test for non-additivity
}
\description{Computes the p-value and various other quantities of interest from Malik's clustering-based test for non-additivity.
}
\usage{
Malikfunction(hfobj, N=1000)
}
\arguments{
  \item{hfobj}{An object of class \code{hiddenf} created by the \code{hiddenf} function}
  \item{N}{The number of Monte Carlo datasets used to determine critical thresholds for Malik's test statistic.  Default value is N=1000.
}
}
\value{A list containing data as named vectors, numbers of factor levels,
vector of simulated test statistics, and corresponding Monte Carlo estimate of the pvalue from Malik's test of additivity.  The standard error of this estimate is inversely proportional to the square root of N.}
\references{
Malik, WA, Mohring, J and Piepho, H. (2014) A clustering-based test for non-additivity in an unreplicated two-way layout, Communications in Statistics-Simulation and Computation, just-accepted.
}
\author{Jason A. Osborne, Christopher T. Franck and Bongseog Choi 
}
\seealso{hiddenf, additivityPvalues
}
\examples{
\dontrun{
library(hiddenf)
data(cjejuni.mtx)
cjejuni.out <- hiddenf(cjejuni.mtx)
cjejuni.Malikout <- Malikfunction(cjejuni.out)
}}
\keyword{non-additivity }
