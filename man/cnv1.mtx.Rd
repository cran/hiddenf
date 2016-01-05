\name{cnv1.mtx}
\alias{cnv1.mtx}
\docType{data}
\title{Copy Number Dataset, Probe #1}
\description{Data from an experiment (courtesy of Matthew Breen, N.C. State University) to study copy number variation in dogs. 
Experiment included thousands of probes, one of which is included here.
}

\usage{data(cnv1.mtx)}
\value{\item{cnv1.mtx}{Matrix of copy number measurements for one specific probe.
Measured for two types of tissue (columns) on each of six dogs (rows) with lymphoma.
}}
\examples{
data(cnv1.mtx)
cnv1.out <- HiddenF(cnv1.mtx)
summary(cnv1.out)
}
\keyword{datasets}
