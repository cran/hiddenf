\name{Graybill.mtx}
\alias{Graybill.mtx}
\docType{data}
\title{Wheat Yields}
\description{
Wheat yields from four genotypes in randomized block design with 13 locations.
}
\usage{data(Graybill.mtx)}
\value{\item{Graybill.mtx}{Matrix of wheat yields, rows are locations, columns are genotypes}
}
\source{
  Graybill, FA. (1954) Variance Heterogeneity in a Randomized Block Design, Biometrics, 10:516-520.
}
\examples{
\dontrun{
data(Graybill.mtx)
Graybill.out <- HiddenF(Graybill.mtx)
plot(Graybill.out) 
}}
\keyword{datasets}
