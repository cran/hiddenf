\name{anova.HiddenF}
\alias{anova.HiddenF}
\alias{anova}
\title{anova function for the class `HiddenF'
}
\description{
Reproduces the analysis of variance table appropriate
to the chosen method of analysis.  The table contains
terms common to the additive model, with additional
terms appropriate to the method of analysis.
For method="ACMIF", additional terms are group, 
group-by-column and row-nested-in-group.  For 
method="Mandel", there is a term for slopes, for
Tukey, there is a term for the multiplicative
coefficient.  For method="KKSA", two anova tables
are given for the two additive models that lead to
the maximally significant F-ratio of error mean 
squares.  }
\usage{
\method{anova}{HiddenF}(object, warncat = TRUE, method = "HiddenF", 
return = FALSE, print = TRUE, stars = FALSE, \dots)
}
\arguments{
  \item{object}{An object of class HiddenF }
  \item{warncat}{A boolean argument that can be used to suppress a warning message about multiplicity adjustment to reported pvalues }
  \item{method}{An argument to specify which test of non-additivity is to be considered }
  \item{return}{A boolean argument determining whether summary statistics are to be returned as a list }
  \item{print}{A boolean argument for whether to display the anova tables }
  \item{stars}{A boolean argument that may be used to suppress the stars in the anova tables }
  \item{\dots}{Additional Arguments }
}
\value{
An object of class `anova'
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
}
\author{
Jason A. Osborne, Bongseog Choi and Christopher T. Franck }
\examples{
data(cjejuni.mtx)
cjejuni.out <- HiddenF(cjejuni.mtx)
anova(cjejuni.out)
anova(cjejuni.out,method="KKSA")}
\keyword{anova}
\keyword{non-additivity}
\keyword{hidden additivity} 
