\name{plot.hiddenf}
\alias{plot.hiddenf}
\alias{plot}
\title{Interaction plot}
\description{Interaction plot with blocks colored according to configuration
that maximizes hidden additivity.}
\usage{
\method{plot}{hiddenf}(x,main,tfactor,bfactor,legendx,\dots)
}
\arguments{
  \item{x}{Object of class 'hiddenf'}
  \item{main}{Plot Title}
  \item{tfactor}{Label of variable (treatment factor) on the horizontal axis}
  \item{bfactor}{Label of trace variable (blocking factor) for the interaction plot}
  \item{legendx}{Graphical parameter that sets the location of the legend (optional) e.g. "topright" or "bottomright"}
  \item{\dots}{ Allows for the use of other graphical parameters for matplot or legend}
}
\author{Christopher T. Franck and Jason A. Osborne 
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{hiddenf}}}
\examples{
data(cjejuni.mtx)
cjejuni.out <- hiddenf(cjejuni.mtx)
plot(cjejuni.out)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Additivity}
