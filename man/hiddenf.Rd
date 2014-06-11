\name{hiddenf}
\alias{hiddenf}
\title{Hiddenf test for matrix data}
\description{
Fits linear model to ymtx, a b-by-t matrix of responses.  Constructs
all possible configurations of rows into two groups, then fits 
full factorial effects model with factors group, column and row, nested in 
group.  Maximum F-ratio for group-by-treatment interaction is reported along
with Bonferroni-corrected pvalue.
}
\usage{
hiddenf(ymtx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ymtx}{
A matrix of responses, with rows corresponding to levels of the
blocking/grouped factor to be put into two groups and columns
corresponding to levels of the treatment/other factor
}
}
\details{Method does not accommodate more than b=15 rows}
\value{List with components
\item{pvalues}{Vector of pvalues, one for each configuration}
\item{pvalue}{(Bonferroni-adjusted) pvalue from configuration
with maximal hidden additivity}
\item{config}{Integer giving the number of the maximal configuration}
\item{config.vector}{Vector of group indicators for maximal configuration}
\item{tall}{List with components y, block, trt}
\item{cc}{Number of possible configurations}
\item{date1}{Time at which the function was called}
\item{date2}{Time at which the function returned this list}
}
\references{
Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.
}
\author{
Jason A. Osborne \email{jaosborn@ncsu.edu}
}
\seealso{anova.hiddenf,summary.hiddenf,print.hiddenf}
\examples{
\dontrun{
data(graybill.mtx)
graybill.hfout <- hiddenf(graybill.mtx)
}
}
