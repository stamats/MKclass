\name{confMatrix}
\alias{confMatrix}
\title{ Compute Confusion Matrix }
\description{
  The function computes the confusion matrix of a binary classification.
}
\usage{
confMatrix(pred, pred.group, truth, namePos, cutoff = 0.5)
}
\arguments{
  \item{pred}{ numeric values that shall be used for classification; e.g. probabilities
  to belong to the positive group. }
  \item{pred.group}{ vector or factor including the predicted group. If missing,
  \code{pred.group} is computed from \code{pred}, where \code{pred >= cutoff} is
  classified as positive.}
  \item{truth}{ true grouping vector or factor. }
  \item{namePos}{ value representing the positive group.}
  \item{cutoff}{ cutoff value used for classification.}
}
\details{
  The function computes the confusion matrix of a binary classification
  consisting of the number of true positive (TP), false negative (FN),
  false positive (FP) and true negative (TN) predictions.
}
\value{
  \code{matrix} with respective numbers of true and false predictions.
}
\references{
  Wikipedia contributors. (2019, July 18). Confusion matrix. In Wikipedia, 
  The Free Encyclopedia. Retrieved 06:00, August 21, 2019, from 
  https://en.wikipedia.org/w/index.php?title=Confusion_matrix&oldid=906886050
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## example from dataset infert
fit <- glm(case ~ spontaneous+induced, data = infert, family = binomial())
pred <- predict(fit, type = "response")

## with group numbers
confMatrix(pred, truth = infert$case, namePos = 1)

## with group names
my.case <- factor(infert$case, labels = c("control", "case"))
confMatrix(pred, truth = my.case, namePos = "case")

## on the scale of the linear predictors
pred2 <- predict(fit)
confMatrix(pred2, truth = infert$case, namePos = 1, cutoff = 0)
}
\keyword{univar}
