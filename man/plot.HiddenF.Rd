\name{plot.HiddenF}
\alias{plot.HiddenF}
\alias{plot}
\title{Interaction plot}
\description{Interaction plot with levels of row factor colored according to 
configuration that maximizes hidden additivity.}
\usage{
\method{plot}{HiddenF}(x,y=NULL,main="Hidden Additivity Plot",
rfactor="Rows Factor",cfactor="Columns Factor", colorvec=c("black","red"),
legendx=FALSE,center=FALSE,\dots)
}
\arguments{
  \item{x}{Object of class 'HiddenF'}
  \item{y}{Deprecated variable not used in this version of plot}
  \item{main}{Plot Title}
  \item{rfactor}{Label of trace variable (row factor) for optional legend of the interaction plot}
  \item{cfactor}{Label of variable (column factor) on the horizontal axis}
  \item{colorvec}{Vector of colors for the two groups in interaction plot}
  \item{legendx}{Graphical parameter that allows for an optional legend, whose
location is determined by point-and-click interface}
  \item{center}{Center the data about the row means}
  \item{\dots}{ Allows for the use of other graphical parameters for matplot or legend}
}
\author{Jason A. Osborne, Christopher T. Franck and Bongseog Choi
}
\seealso{\code{\link{HiddenF}}}
\examples{
data(cjejuni.mtx)
cjejuni.out <- HiddenF(cjejuni.mtx)
plot(cjejuni.out)
}
\keyword{interaction plot}
\keyword{hidden additivity}
