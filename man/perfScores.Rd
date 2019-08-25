\name{perfScores}
\alias{perfScores}
\title{ Compute Performance Scores for Binary Classification }
\description{
  The function computes various performance scores for binary classification.
}
\usage{
perfScores(pred, truth, namePos, weight = 0.5, wBS = weight, digits = 3)
}
\arguments{
  \item{pred}{ numeric values that shall be used for classification; e.g. probabilities
  to belong to the positive group. }
  \item{truth}{ true grouping vector or factor. }
  \item{namePos}{ value representing the positive group.}
  \item{weight}{ weight used for computing weighted values. Must be in [0,1].}
  \item{wBS}{ weight used for computing the weighted Brier score (BS), where
postive BS is multiplied by \code{wBS} and negative BS by \code{1-wBS}. Must be in [0,1].}
  \item{digits}{ integer indicating the number of decimal places returned by applying (round).}
}
\details{
  The function \code{perfScores} computes various performance Scores.
  The scores are:
  area under the ROC curve (AUC), Gini index, Brier score, positive Brier score,
  negative Brier score, weighted Brier score, and balanced Brier score.

  If the predictions (\code{pred}) are not in the interval [0,1] the standard
  logistic function is applied to transform the values of \code{pred - cutoff}
  to [0,1].
}
\value{
  \code{data.frame} with names of the scores and their respective values.
}
\references{
  G.W. Brier (1950). Verification of forecasts expressed in terms of probability.
  \emph{Mon. Wea. Rev.} \bold{78}, 1-3.

  T. Fawcett (2006). An introduction to ROC analysis.
  \emph{Pattern Recognition Letters} \bold{27}, 861-874.

  T.A. Gerds, T. Cai, M. Schumacher (2008). The performance of risk prediction
  models. \emph{Biom J} \bold{50}, 457-479.

  D. Hand, R. Till (2001). A simple generalisation of the area under the ROC
  curve for multiple class classification problems.
  \emph{Machine Learning} \bold{45}, 171-186.

  J. Hernandez-Orallo, P.A. Flach, C. Ferri (2011). Brier curves: a new cost-
  based visualisation of classifier performance. In L. Getoor and T. Scheffer (eds.)
  \emph{Proceedings of the 28th International Conference on Machine Learning} (ICML-11),
  585???592 (ACM, New York, NY, USA).

  J. Hernandez-Orallo, P.A. Flach, C. Ferri (2012). A unified view of performance
  metrics: Translating threshold choice into expected classification loss.
  \emph{J. Mach. Learn. Res.} \bold{13}, 2813-2869.

  B.W. Matthews (1975). Comparison of the predicted and observed secondary
  structure of t4 phage lysozyme. \emph{Biochimica et Biophysica Acta} (BBA) -
  Protein Structure \bold{405}, 442-451.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## example from dataset infert
fit <- glm(case ~ spontaneous+induced, data = infert, family = binomial())
pred <- predict(fit, type = "response")

## with group numbers
perfScores(pred, truth = infert$case, namePos = 1)

## with group names
my.case <- factor(infert$case, labels = c("control", "case"))
perfScores(pred, truth = my.case, namePos = "case")

## on the scale of the linear predictors
pred2 <- predict(fit)
perfScores(pred2, truth = infert$case, namePos = 1)

## using weights
perfScores(pred, truth = infert$case, namePos = 1, weight = 0.3)
}
\keyword{univar}
