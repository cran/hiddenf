\name{cjejuni.mtx}
\alias{cjejuni.mtx}
\docType{data}
\title{Annual prevalence of C.jejuni strain of Campylobacter}
\description{
Data are courtesy of Dr. Sophia Kathariou and Yucan Liu, North Carolina
State University.  The entries in the matrix are fractions of campylobacter
strains sampled that were classified as C.jejuni.  Data were
collected  over 5 year period across four turkey plants in North Carolina.  
Rows are plants, columns are years 2008-2012.
}
\usage{data(cjejuni.mtx)}
\value{matrix of C.jejuni fractions}
\examples{
data(cjejuni.mtx)
cjejuni.out <- HiddenF(cjejuni.mtx)
plot(cjejuni.out)
}
\keyword{datasets}
