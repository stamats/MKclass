\name{perfMeasures}
\alias{perfMeasures}
\alias{ACC}
\alias{PCC}
\alias{PMC}
\alias{errorRate}
\alias{sensitivity}
\alias{specificity}
\alias{recall}
\alias{prevalence}
\alias{NIR}
\alias{WACC}
\alias{BACC}
\alias{informedness}
\alias{YoudenJ}
\alias{PLR}
\alias{NLR}
\alias{WLR}
\alias{BLR}
\alias{PPV}
\alias{NPV}
\alias{WPV}
\alias{BPV}
\alias{precision}
\alias{markedness}
\alias{Fbeta}
\alias{F1score}
\alias{DSC}
\alias{JSC}
\alias{SMC}
\alias{RI}
\alias{MCC}
\alias{PPP}
\alias{EACC}
\alias{kappa}
\alias{detectionRate}
\title{ Compute Performance Measures or Binary Classification }
\description{
  The function computes various performance measures for binary classification.
}
\usage{
perfMeasures(pred, pred.group, truth, namePos, cutoff = 0.5,
             weight = 0.5, wACC = weight, wLR = weight, 
             wPV = weight, digits = 3)
ACC(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
PCC(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
PMC(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
errorRate(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
sensitivity(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
specificity(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
recall(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
prevalence(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
NIR(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
WACC(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3, weight = 0.5) 
BACC(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
informedness(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
YoudenJ(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
PLR(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
NLR(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
WLR(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3, weight = 0.5)
BLR(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
PPV(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
NPV(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
WPV(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3, weight = 0.5)
BPV(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3, weight = 0.5)
precision(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
markedness(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
Fbeta(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3, beta = 1)
F1score(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
DSC(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
JSC(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
SMC(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
RI(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
MCC(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
PPP(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
EACC(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
kappa(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
detectionRate(pred, pred.group, truth, namePos, cutoff = 0.5, digits = 3)
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
  \item{weight}{ weight used for computing weighted values. Must be in [0,1].}
  \item{wACC}{ weight used for computing the weighted accuracy, where sensitivity is multiplied by \code{wACC} and specificity by \code{1-wACC}. Must be in [0,1].}
  \item{wLR}{ weight used for computing the weighted likelihood ratio, where PLR is multiplied by \code{wLR} and NLR by \code{1-wLR}. Must be in [0,1].}
  \item{wPV}{ weight used for computing the weighted predictive value, where PPV is multiplied by \code{wPV} and NPV by \code{1-wPV}. Must be in [0,1].}
  \item{digits}{ integer indicating the number of decimal places returned by applying (round).}
}
\details{
  The function \code{perfMeasures} computes various performance measures.
  The measures are:
  accuracy (ACC), probabiliy of correct classification (PCC), probability of
  missclassification (PMC), error rate, sensitivity, specificity, recall, 
  prevalence, no information rate (NIR), 
  weighted accuracy (wACC), balanced accuracy (BACC),
  (Bookmarker) informedness, Youden's J statistic, 
  positive likelihood ratio (PLR), negative likelihood ratio (NLR), 
  weighted likelihood ratio (WLR), balanced likelihood ratio (BLR), 
  positive predictive value (PPV), negative predictive value (NPV),
  weighted predictive value (WPV), balanced predictive value (BPV), 
  precision, markedness, Fbeta score, F1 score, 
  Dice similarity coefficient (DSC), Jaccard similarity coefficient (JSC),
  Rand index (RI), Matthews' correlation coefficient (MCC),
  proportion of positive predictions (PPP), expected accuracy (EACC),
  Cohen's kappa coefficient, and detection rate.

  These performance measures have in common that they require a dichotomization
  (discretization) of a computed continuous classification function.
}
\value{
  \code{data.frame} with names of the performance measures and their 
  respective values.
}
\references{
  K.H. Brodersen, C.S. Ong, K.E. Stephan, J.M. Buhmann (2010). The balanced
  accuracy and its posterior distribution. In \emph{Pattern Recognition} (ICPR),
  20th International Conference on, 3121-3124 (IEEE, 2010).

  J.A. Cohen (1960). A coefficient of agreement for nominal scales.
  \emph{Educational and Psychological Measurement} \bold{20}, 3746.

  T. Fawcett (2006). An introduction to ROC analysis.
  \emph{Pattern Recognition Letters} \bold{27}, 861-874.

  T.A. Gerds, T. Cai, M. Schumacher (2008). The performance of risk prediction
  models. \emph{Biom J} \bold{50}, 457-479.

  D. Hand, R. Till (2001). A simple generalisation of the area under the ROC
  curve for multiple class classification problems.
  \emph{Machine Learning} \bold{45}, 171-186.

  J. Hernandez-Orallo, P.A. Flach, C. Ferri (2012). A unified view of performance
  metrics: Translating threshold choice into expected classification loss.
  \emph{J. Mach. Learn. Res.} \bold{13}, 2813-2869.

  B.W. Matthews (1975). Comparison of the predicted and observed secondary
  structure of t4 phage lysozyme. \emph{Biochimica et Biophysica Acta} (BBA) -
  Protein Structure \bold{405}, 442-451.

  D.M. Powers (2011). Evaluation: From Precision, Recall and F-Factor to ROC,
  Informedness, Markedness and Correlation. \emph{Journal of Machine Learning
  Technologies} \bold{1}, 37-63.

  N.A. Smits (2010). A note on Youden's J and its cost ratio.
  \emph{BMC Medical Research Methodology} \bold{10}, 89.

  B. Wallace, I. Dahabreh (2012). Class probability estimates are unreliable for
  imbalanced data (and how to fix them). In \emph{Data Mining} (ICDM), IEEE 12th
  International Conference on, 695-04.

  J.W. Youden (1950). Index for rating diagnostic tests.
  \emph{Cancer} \bold{3}, 32-35.
}
\author{ Matthias Kohl \email{Matthias.Kohl@stamats.de}}
\examples{
## example from dataset infert
fit <- glm(case ~ spontaneous+induced, data = infert, family = binomial())
pred <- predict(fit, type = "response")

## with group numbers
perfMeasures(pred, truth = infert$case, namePos = 1)

## with group names
my.case <- factor(infert$case, labels = c("control", "case"))
perfMeasures(pred, truth = my.case, namePos = "case")

## on the scale of the linear predictors
pred2 <- predict(fit)
perfMeasures(pred2, truth = infert$case, namePos = 1, cutoff = 0)

## using weights
perfMeasures(pred, truth = infert$case, namePos = 1, weight = 0.3)
}
\keyword{univar}
