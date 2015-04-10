\name{anova.hiddenf}
\alias{anova.hiddenf}
\alias{anova}
\title{Anova function for the class `hiddenf'
}
\description{
Reproduces the analysis of variance table corresponding
to the configuration of levels of a blocking/grouped
factor into two groups such that interaction between 
group and the treatment/other factor is maximized
}
\usage{
\method{anova}{hiddenf}(object, warncat=TRUE, \dots)
}
\arguments{
  \item{object}{An object of class "hiddenf"}
  \item{warncat}{A boolean argument that can be used to suppress warning about multiplicity adjustment to reported pvalue}
  \item{\dots}{other arguments}
}
\value{
\item{anova.out}{An object of class `anova'} 
}
\references{
Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.
}
\author{
Jason A. Osborne, Bongseog Choi and Christopher T. Franck
}
\seealso{
\code{\link{anova}}, \code{\link{lm}}
}
\examples{
data(cjejuni.mtx)
cjejuni.out <- hiddenf(cjejuni.mtx)
anova(cjejuni.out)
}
\keyword{anova}
\keyword{non-additivity}
\keyword{hidden additivity}
