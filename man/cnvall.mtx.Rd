\name{cnvall.mtx}
\alias{cnvall.mtx}
\docType{data}
\title{Copy Number Variation}
\description{Data, courtesy of Dr. Matthew Breen, N.C. State University, from 
an experiment to study copy number variation in dogs.  Experiment included 
thousands of probes, several of which are included here,
and indexed by the variable called `dataset'.
}

\usage{data(cnvall.mtx)}
\value{\item{cnvall.mtx}{Matrix of copy number measurements for several probes.
Measured for two types of tissue (columns) on each of six dogs (rows) with lymphoma.  Copy Number measurements are one column in the matrix and it
is not formatted for functions in 'hiddenf' that require matrix input.
}}
\examples{
\dontrun{
data(cnvall.mtx)
cnvall.mtx
cnv3.mtx <- matrix(cnvall.mtx[25:36,3],byrow=T,nrow=12,ncol=2)
cnv3.out <- hiddenf.mtxfcn(cnv3.mtx)
print(cnv3.out$pvalue)
anova(cnv3.out)
}}
\keyword{datasets}
