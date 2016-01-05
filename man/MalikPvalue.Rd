\name{MalikPvalue}
\alias{MalikPvalue}
\title{Malik's test for non-additivity
}
\description{Computes the p-value from the clustering-based test for 
non-additivity developed in Malik, et al. (2015).
}
\usage{
MalikPvalue(hfobj, N=500,pnote=TRUE)
}
\arguments{
  \item{hfobj}{An object of class \code{HiddenF} created by the \code{HiddenF} function}
  \item{N}{The number of Monte Carlo datasets used to determine critical thresholds for Malik's test statistic.  Default value is N=500
}
  \item{pnote}{Boolean variable that can be used to suppress note about
number of Monte Carlo datasets used to estimate pvalue
}
}
\value{A Monte Carlo estimate of the p-value from the Malik et al (2015) test of non-additivity.  The standard error of this estimate is inversely proportional to the square root of N.}
\references{
Malik, WA, Mohring, J and Piepho, H. (2014) A clustering-based test for non-additivity in an unreplicated two-way layout, Communications in Statistics-Simulation and Computation.
}
\author{Jason A. Osborne, Christopher T. Franck and Bongseog Choi 
}
\seealso{HiddenF, additivityPvalues
}
\examples{
\dontrun{
library(hiddenf)
data(cjejuni.mtx)
cjejuni.out <- HiddenF(cjejuni.mtx)
cjejuni.MalikPvalue <- MalikPvalue(cjejuni.out)
}}
\keyword{non-additivity }
