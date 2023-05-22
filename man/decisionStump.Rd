\name{decisionStump}
\alias{decisionStump}
\title{ Compute Decision Stumps }
\description{
  The function computes a decision stump for binary classification also known 
  as 1-level decision tree or 1-rule.
}
\usage{
decisionStump(pred, truth, namePos, perfMeasure = "YJS",
          MAX = TRUE, parallel = FALSE, ncores, delta = 0.01, ...)
}
\arguments{
  \item{pred}{ numeric values that shall be used for classification; e.g. probabilities
  to belong to the positive group. }
  \item{truth}{ true grouping vector or factor. }
  \item{namePos}{ value representing the positive group; i.e., the name of the 
  category where one expects higher values for \code{pred}.}
  \item{perfMeasure}{ a single performance measure computed by function \code{perfMeasure}.}
  \item{MAX}{logical value. Whether to maximize or minimize the performacne
  measure.}
  \item{parallel}{ logical value. If \code{TRUE} packages foreach and doParallel
  are used to parallelize the computations.}
  \item{ncores}{integer value, number of cores that shall be used to parallelize
  the computations.}
  \item{delta}{numeric value for setting up grid for optimization; start is 
  minimum of \code{pred-delta}, end is maximum of \code{pred+delta}.}
  \item{...}{further arguments passed to function \code{prefMeasures}.}
}
\details{
  The function is able to compute a decision stump for various performance
  measures, all performance measures that are implemented in function
  \code{perfMeasures}. Of course, for several of them the computation is 
  not really usefull such as sensitivity or specificity where one will get
  trivial decision rules. 
  
  In addition, a decision stump will only give a meaningful result if there is
  a monotone relationship between the two categories and the numeric values 
  given in \code{pred}. In such a case the name of the category where one expects 
  higher values should be given in \code{namePos}.
}
\value{
  Object of class \code{decisionStump}.
}
\references{
  W. Iba and P. Langley (1992). Induction of One-Level Decision Trees. 
  In: \emph{Machine Learning Proceedings 1992}, pages 233-240.
  URL: https://doi.org/10.1016/B978-1-55860-247-2.50035-8  

  R.C. Holte (1993). Very simple classification rules perform well on most 
  commonly used datasets. In: \emph{Machine Learning}, pages 63-91.
  URL: https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.67.2711
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## example from dataset infert
fit <- glm(case ~ spontaneous+induced, data = infert, family = binomial())
pred <- predict(fit, type = "response")
res <- decisionStump(pred, truth = infert$case, namePos = 1)
predict(res, newdata = seq(from = 0, to = 1, by = 0.1))
}
\keyword{univar}
