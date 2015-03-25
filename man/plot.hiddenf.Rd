\name{plot.hiddenf}
\alias{plot.hiddenf}
\alias{plot}
\title{Interaction plot}
\description{Interaction plot with blocks colored according to configuration
that maximizes hidden additivity.}
\usage{
\method{plot}{hiddenf}(x,y=NULL,main="Hidden Additivity Plot",tfactor="Treatment Factor",
bfactor="Blocking Factor",colorvec=c("black","red"),legendx=FALSE,center=FALSE,\dots)
}
\arguments{
  \item{x}{Object of class 'hiddenf'}
  \item{y}{Deprecated variable not used in this version of plot}
  \item{main}{Plot Title}
  \item{tfactor}{Label of variable (treatment factor) on the horizontal axis}
  \item{bfactor}{Label of trace variable (blocking factor) for optional legend of the interaction plot}
  \item{colorvec}{Vector of colors for the two groups in interaction plot}
  \item{legendx}{Graphical parameter that allows for an optional legend, whose
location is determined by point-and-click interface}
  \item{center}{Center the data about the row means}
  \item{\dots}{ Allows for the use of other graphical parameters for matplot or legend}
}
\author{Jason A. Osborne, Christopher T. Franck and Bongseog Choi
}
\seealso{\code{\link{hiddenf}}}
\examples{
data(cjejuni.mtx)
cjejuni.out <- hiddenf(cjejuni.mtx)
plot(cjejuni.out)
}
\keyword{interaction plot}
\keyword{hidden additivity}
