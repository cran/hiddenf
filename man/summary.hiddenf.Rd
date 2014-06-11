\name{summary.hiddenf}
\alias{summary.hiddenf}
\alias{summary}
\title{ Summary function for the class "hiddenf"
} 
\description{
Summarize the results of the hidden F-test for nonadditivity} 
\usage{
\method{summary}{hiddenf}(object, \dots)
}
\arguments{
\item{object}{An object of class "hiddenf"}
\item{\dots}{other arguments}
}
\value{
\item{group1}{List of levels of blocking/grouped factor in group 1}
\item{group2}{List of levels of blocking/grouped factor in group 2}
}
\references{
Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.

}
\author{
Jason A. Osborne
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{hiddenf}
}
\examples{
data(Boik.mtx)
Boik.out <- hiddenf(Boik.mtx)
summary(Boik.out)
}
\keyword{ ~interaction }
\keyword{ ~hidden additivity }
