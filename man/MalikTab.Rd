\name{MalikTab}
\alias{MalikTab}
\title{Malik's critical values
}
\description{
This function computes Monte Carlo estimates of critical values
for Malik's test for non-additivity at significance levels .01,.05 and .1
}
\usage{
MalikTab(t, b, N)
}
\arguments{
  \item{t}{Number of levels of column factor
}
  \item{b}{Number of levels of row factor
}
  \item{N}{Number of additive datasets to be generated for Monte Carlo
estimation of critical values
}
}
\value{
A list with several components:
\item{Tcsim}{a random sample of N test statistics from Malik's procedure
under the hypothesis of additivity}
\item{q}{a vector with first two elements equal to the number of levels
of the row and column factors, along with the 99th, 95th and 90th
quantiles from the random sample}
}
\references{
Malik, WA, Mohring, J and Piepho, H. (2014) A clustering-based test for
non-additivity in an unreplicated two-way layout, Communications in 
Statistics-Simulation and Computation, just-accepted
}
\author{Jason A. Osborne,  Christopher T. Franck and Bongseog Choi
}
\seealso{MalikPvalue
}
\examples{
# get critical values to conduct Malik's test of additivity
# in an experiment with row and column factors with 4 and 5 levels,
# respectively
\dontrun{
data(cjejuni.mtx)
cjejuni.out <- hiddenf(cjejuni.mtx)
Malik.pvalue <- MalikPvalue(cjejuni.out)
cjejuni.Malikobj <- Maliktab(4,5,N=1000)
print(cjejuni.Malikobj$q)
}}
\keyword{ non-additivity }
