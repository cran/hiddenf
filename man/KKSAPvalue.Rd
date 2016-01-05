\name{KKSAPvalue}
\alias{KKSAPvalue}
\title{Kharrati-Kopaei and Sadooghi-Alvandi's test for non-additivity
}
\description{Reports the p-value from Kharrati-Kopaei and Sadooghi-Alvandi's test for non-additivity.  This procedure searches over all configurations of
rows of the input matrix into two non-empty sets, each having at least
two elements.  Separate linear models in which row and column effects
are additive are fit to each set, and the configuration with maximum
ratio of error mean squares is reported, along with a p-value.
}
\usage{
KKSAPvalue(hfobj)
}
\arguments{
  \item{hfobj}{
An object of class \code{HiddenF} created by the \code{HiddenF} function
}
}
\details{
Requires that data matrix has more than four rows (r > 4)
}
\value{
A list containing the input data matrix converted to list form,
a numeric p-value from a test of the hypothesis of additivity,
and a vector giving the corresponding configuration of rows into 
two groups.
}
\references{
Kharrati-Kopaei, M. and Sadooghi-Alvandi, SM. (2007).
A New Method for Testing Interaction in Unreplicated
Two-Way Analysis of Variance, Communications in Statistics - Theory
and Methods, 36:2787-2803.
}
\author{
Jason A. Osborne, Christopher T. Franck and  Bongseog Choi
}
\seealso{
HiddenF, additivityPvalues
}
\examples{
library(hiddenf)
data(cjejuni.mtx)
cjejuni.out <- HiddenF(cjejuni.mtx)
KKSA.out <- KKSAPvalue(cjejuni.out)
print(KKSA.out$pvalue)
}
\keyword{non-additivity}
