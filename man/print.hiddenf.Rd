\name{print.hiddenf}
\alias{print.hiddenf}
\alias{print}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Printing hiddenf objects}
\description{`print' method for class `hiddenf'
}
\usage{
\method{print}{hiddenf}(x, ...)
}
\arguments{
  \item{x}{
An object of class `hiddenf'
}
  \item{\dots}{further arguments passed from other methods}
}
\value{
\item{Fratio}{The F-statistic for group-by-treatment interaction}
item{adjpvalue}{The corresponding Bonferroni-adjusted p-value}
%% ...
}
\references{
Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.
}
\author{
Jason A. Osborne and Christopher T. Franck
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
The function which searches for the maximal F-ratio, \code{hiddenf}
}
\examples{
data(cnv1.mtx)
cnv1.out <- hiddenf(cnv1.mtx)
print(cnv1.out)
}
\keyword{ ~interaction }
\keyword{ ~nonadditivity }
