\name{TukeyPvalue}
\alias{TukeyPvalue}
\title{Tukey's single degree of freedom test for nonadditivity
}
\description{Reports the p-value from Tukey's single degree of freedom test for non-additivity
}
\usage{
TukeyPvalue(hfobj)
}
\arguments{
  \item{hfobj}{
An object of class \code{hiddenf} created by the \code{hiddenf} function
}
}
\value{
A list with two components: (1) a numeric p-value from Tukey's single degree of freedom test of the hypothesis of additivity and (2) an object of class `lm' corresponding to the linear model additive in row and column effects.
}
\references{
Tukey, JW (1949).  One Degree of Freedom for Non-Additivity.
Biometrics, 5:232-242.
}
\author{
Jason A. Osborne, Christopher T. Franck and Bongseog Choi
}
\seealso{
additivityPvalues
}
\examples{
library(hiddenf)
data(cjejuni.mtx)
cjejuni.out <- HiddenF(cjejuni.mtx)
tukey.pvalue <- TukeyPvalue(cjejuni.out)
}
\keyword{non-additivity}
