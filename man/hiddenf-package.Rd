\name{hiddenf-package}
\alias{hiddenf-package}
\docType{package}
\title{
Tests for nonadditivity using the hidden F test 
}
\description{
Fits a linear model to a (b-by-t) matrix of responses.  Includes factorial 
effects of two factors, with b rows of matrix as one blocking/grouped 
factor with b levels and columns as t levels of a treatment/other factor.
Configurations formed by placing rows into two groups and 
creating a third grouping factor.  Linear models are fit for all 
c=2^(b-1)-1 possible configurations.  The resulting pvalue for 
group-by-treatment interaction is reported, after Bonferroni 
correction for multiplicity of configurations.
}
\details{
\tabular{ll}{
Package: \tab hiddenf\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-06-05\cr
License: \tab GPL-2\cr
}
}
\author{
Jason A. Osborne and Christopher T. Franck
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
cnv1.hfout <- hiddenf(cnv1.mtx)
anova(cnv1.hfout) 
}
