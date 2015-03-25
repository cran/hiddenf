\name{hiddenf-package}
\alias{hiddenf-package}
\docType{package}
\title{
The all-Configurations, Maximum-Interaction F-Test for Hidden Additivity
}
\description{
Fits a linear model to a (b-by-t) matrix of responses.  Includes factorial 
effects of two factors, with matrix rows as one blocking/grouped 
factor with b levels and matrix columns as t levels of a treatment/other factor.
Configurations formed by placing rows into two groups and 
creating a third grouping factor.  Linear models are fit for all 
c=2^(b-1)-1 possible configurations.  The resulting pvalue for 
group-by-treatment interaction is reported, after Bonferroni 
correction for multiplicity of configurations.  An object of class `hiddenf'
is returned than can be used as an argument for plot(), print(), summary() and
anova() functions and to compute p-values from other tests of 
non-additivity, using other functions included in the package.
}
\details{
\tabular{ll}{
Package: \tab hiddenf\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2015-03-06\cr
License: \tab GPL-2\cr
}
}
\author{
Jason A. Osborne, Christopher T. Franck and Bongseog Choi
Maintainer: Jason A. Osborne <jaosborn@ncsu.edu>
}
\references{
Tukey, JW (1949).  One Degree of Freedom for Non-Additivity.
Biometrics, 5:232-242. 

Mandel J. (1961) Non-Additivity in Two-Way Analysis of Variance,
Journal of the American Statistical Association, 56:878-888.

Kharrati-Kopaei, M. and Sadooghi-Alvandi, SM. (2007).
A New Method for Testing Interaction in Unreplicated
Two-Way Analysis of Variance, Communications in Statistics - Theory
and Methods, 36:2787-2803 .

Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.

Malik, WA, Mohring, J and Piepho, H. (2015) A clustering-based test for non-additivity in an unreplicated two-way layout, Communications in Statistics-Simulation and Computation.
}
\examples{
data(cnv1.mtx)
cnv1.hfout <- hiddenf(cnv1.mtx)
anova(cnv1.hfout) 
}
