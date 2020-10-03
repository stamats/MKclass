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
  \item{namePos}{ value representing the positive group.}
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
}
\value{
  Object of class \code{decisionStump}.
}
%\references{}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## example from dataset infert
fit <- glm(case ~ spontaneous+induced, data = infert, family = binomial())
pred <- predict(fit, type = "response")
decisionStump(pred, truth = infert$case, namePos = 1)
}
\keyword{univar}
