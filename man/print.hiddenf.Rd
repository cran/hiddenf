\name{print.hiddenf}
\alias{print.hiddenf}
\alias{print}
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
  \item{\dots}{further arguments}
}
\value{
\item{Fratio}{The F-statistic for group-by-treatment interaction}
\item{adjpvalue}{The corresponding Bonferroni-adjusted p-value}
%% ...
}
\references{
Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.
}
\author{
Jason A. Osborne, Christopher T. Franck and Bongseog Choi
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
hiddenf
}
\examples{
data(cnv1.mtx)
cnv1.out <- hiddenf(cnv1.mtx)
print(cnv1.out)
}
\keyword{non-additivity}
