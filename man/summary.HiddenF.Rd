\name{summary.HiddenF}
\alias{summary.HiddenF}
\alias{summary}
\title{Summary function for the class "HiddenF" }
\description{Summarize the results of the ACMIF test for nonadditivity}
\usage{
\method{summary}{HiddenF}(object, method="HiddenF",\dots)
}
\arguments{
  \item{object}{An object of class "HiddenF"}
  \item{method}{the method to be used; if "ACMIF", the configuration with maximal hidden additivity is printed along with the mean response for each column after grouping rows according to this maximal configuration.  No summary generated for other methods}
  \item{\dots}{other arguments} }
\value{
\item{group1}{Vector of levels of row factor in group 1}
\item{group2}{Vector of levels of row factor in group 2}
\item{grp1means}{Vector of column means among rows in group 1}
\item{grp2means}{Vector of column means among rows in group 1}
}
\references{
Franck CT, Nielsen, DM and Osborne, JA. (2013) A Method for Detecting Hidden
Additivity in two-factor Unreplicated Experiments, Computational Statistics
and Data Analysis, 67:95-104.
}
\author{
Christopher T. Franck and Jason A. Osborne
}
\seealso{
\code{HiddenF}
}
\examples{
data(Boik.mtx)
Boik.out <- HiddenF(Boik.mtx)
Boik.summary <- summary(Boik.out)
}
\keyword{ ~nonadditivity }
\keyword{ ~hidden additivity }
