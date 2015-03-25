\name{hiddenf}
\alias{hiddenf}
\title{Hiddenf test for matrix data}
\description{
Fits linear model to ymtx, a matrix of responses from two-factor layouts.  Constructs all possible configurations of rows(=blocks) into two groups, then fits full factorial effects model with factors group, column(=treatment) and row-nested-within-group.  Maximum F-ratio for group-by-treatment interaction is reported along with Bonferroni-corrected p-value.
}
\usage{
hiddenf(ymtx)
}
\arguments{
  \item{ymtx}{
A matrix of responses, with rows corresponding to levels of the
blocking/grouped factor to be put into two groups and columns
corresponding to levels of the treatment/other factor
}
}
\details{Method does not accommodate more than b=20 rows}
\value{List-object of class `hiddenf' with components
\item{pvalues}{Vector of pvalues, one for each configuration}
\item{pvalue}{(Bonferroni-adjusted) pvalue from configuration
with maximal hidden additivity}
\item{config}{Integer giving the number of the maximal configuration}
\item{config.vector}{Vector of group indicators for maximal configuration}
\item{tall}{List with components y, block, trt}
\item{cc}{Number of possible configurations}
}
\references{
Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.
}
\author{
Jason A. Osborne \email{jaosborn@ncsu.edu}, Christopher T. Franck and Bongseog Choi
}
\seealso{plot.hiddenf, anova.hiddenf, summary.hiddenf, print.hiddenf}
\examples{
data(cjejuni.mtx)
cjejuni.out <- hiddenf(cjejuni.mtx)
summary(cjejuni.out)
}
\keyword{non-additivity}
