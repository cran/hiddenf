\name{hiddenf-package}
\alias{hiddenf-package}
\docType{package}
\title{
Tests for nonadditivity using the hidden F test 
}
\description{
Fits a linear model to a (r-by-c) matrix of responses.  Includes factorial effects of two factors, with rows of the matrix as one factor with r levels and columns as c levels of another factor.  Configurations formed by placing rows into two groups and creating a third grouping factor.  Linear models are fit for all b=2^(r-1)-1 possible configurations.  The resulting pvalue for group-by-treatment interaction is reported, after Bonferroni correction for multiplicity of configurations.
}
\details{
\tabular{ll}{
Package: \tab hiddenf\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2015-10-12\cr
License: \tab GPL-2\cr
}
}
\author{
Jason A. Osborne, Christopher T. Franck and Bongseog Choi
Maintainer: Jason A. Osborne <jaosborn@ncsu.edu>
}
\references{
Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.
}
\keyword{ package }
\examples{
data(cnv1.mtx)
cnv1.out <- HiddenF(cnv1.mtx)
anova(cnv1.out) 
}
