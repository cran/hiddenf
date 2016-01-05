\name{HiddenF}
\alias{HiddenF}
\title{Hidden F function for matrix data
}
\description{
Fits linear model to ymtx, a matrix of responses of dimension r-by-c.  Constructs all possible configurations of rows into two non-empty groups, then, for each configuration, fits full factorial effects models with three factors for group, group-by-column, row and row nested within column.  The maximum F-ratio for group-by-column interaction is reported along with Bonferroni-adjusted p-value.
}
\usage{
HiddenF(ymtx)
}
\arguments{
  \item{ymtx}{
A matrix of responses, with rows corresponding to levels of one factor, and columns the levels of a second factor
}
}
\value{List-object of class `HiddenF' with components
\item{adjpvalue}{(Bonferroni-adjusted) pvalue from configuration with maximal hidden additivity}
\item{config.vector}{Vector of group indicators for configuration with maximal hidden additivity}
\item{tall}{A list with components y, row, col}
\item{cc}{Number of possible configurations} }
\references{
Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.}
\author{
Jason A. Osborne \email{jaosborn@ncsu.edu}, Christopher T. Franck and
Bongseog Choi }
\seealso{\code{\link{summary.HiddenF}}}
\examples{
library(hiddenf)
data(cjejuni.mtx)
cjejuni.out <- HiddenF(cjejuni.mtx)
summary(cjejuni.out) }
\keyword{anova}
