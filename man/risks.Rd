\name{risks}
\alias{risks}
\title{ Compute RR, OR and Other Risk Measures }
\description{
  The function computes relative risk (RR), odds ration (OR), and several
  other risk measures; see details.
}
\usage{
risks(p0, p1)
}
\arguments{
  \item{p0}{ numeric vector of length 1: incidence of the outcome of interest in
             the nonexposed group.}
  \item{p1}{ numeric vector of length 1: incidence of the outcome of interest in
             the exposed group.}
}
\details{
  The function computes relative risk (RR), odds-ratio (OR), relative risk
  reduction (RRR) resp. relative risk increase (RRI), absolute risk reduction (ARR)
  resp. absolute risk increase (ARI), number needed to treat (NNT) resp.
  number needed to harm (NNH).
}
\value{
  Vector including several risk measures.
}
\references{
  Porta, M. (2014). A Dictionary of Epidemiology. Oxford University Press. 
  Retrieved 3 Oct. 2020, from 
  \url{https://www.oxfordreference.com/view/10.1093/acref/9780199976720.001.0001/acref-9780199976720}
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## See worked example in Wikipedia
risks(p0 = 0.4, p1 = 0.1)
risks(p0 = 0.4, p1 = 0.5)
}
\keyword{univar}
