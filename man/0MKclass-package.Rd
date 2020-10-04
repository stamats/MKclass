\name{MKclass-package}
\alias{MKclass-package}
\alias{MKclass}
\docType{package}
\title{
Statistical Classification.
}
\description{
Performance measures and scores for statistical classification such as accuracy, 
sensitivity, specificity, recall, similarity coefficiencts, AUC, GINI index, 
Brier score and many more. Calculation of optimal cut-offs and decision stumps 
(Iba and Langley (1991), <doi:10.1016/B978-1-55860-247-2.50035-8>) for the all 
implemented performance measures. Hosmer-Lemeshow goodness of fit tests 
(Lemeshow and Hosmer (1982), <doi:10.1093/oxfordjournals.aje.a113284>; 
Hosmer et al (1997), <doi:10.1002/(SICI)1097-0258(19970515)16:9\%3C965::AID-SIM509\%3E3.0.CO;2-O>). 
Statistical and epidemiological risk measures such as relative risk, odds ratio, 
number needed to treat (Porta (2014), <doi:10.1093\%2Facref\%2F9780199976720.001.0001>).
}
\details{
\tabular{ll}{
Package: \tab MKclass\cr
Type: \tab Package\cr
Version: \tab 0.4\cr
Date: \tab 2020-10-03\cr
Depends: \tab R(>= 4.0.0)\cr
Imports: \tab stats\cr
Suggests: \tab knitr, rmarkdown, foreach, parallel, doParallel\cr
License: \tab LGPL-3\cr
URL: \tab http://www.stamats.de/\cr
}
library(MKclass)
}
\author{ Matthias Kohl \url{http://www.stamats.de}

Maintainer: Matthias Kohl  \email{matthias.kohl@stamats.de}}
%\references{
%~~ Literature or other references for background information ~~
%}
%~~ Optionally other keywords from doc/KEYWORDS, one per line
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%~~ simple examples of the most important functions ~~
%}
\keyword{package}
