\name{perfMeasures}
\alias{perfMeasures}
\alias{ACC}\alias{PCC}\alias{FC}\alias{SMC}\alias{RSI}\alias{PMC}\alias{ER}
\alias{FIC}\alias{BACC}\alias{BFG0}\alias{BFG1}\alias{BLR}\alias{BPV}
\alias{CKC}\alias{CRV}\alias{CSI}\alias{DFM}\alias{DOR}\alias{DP}
\alias{DPREV}\alias{DPp}\alias{DR}\alias{DSC}\alias{EACC}\alias{EC}
\alias{F1S}\alias{FBS}\alias{FDR}\alias{FNR}\alias{FO}\alias{FOR}
\alias{FPR}\alias{HR}\alias{INF}\alias{INFQR}\alias{JD}\alias{JE2}
\alias{JSC}\alias{MARK}\alias{MCC}\alias{MI2}\alias{MR}\alias{NIR}
\alias{NLR}\alias{NPOSTO}\alias{NPOSTP}\alias{NPREO}\alias{NPREP}
\alias{NPV}\alias{NU}\alias{PD}\alias{PFA}\alias{PHIC}\alias{PLR}
\alias{POSTO}\alias{POSTP}\alias{PPP}\alias{PPV}\alias{PREC}
\alias{PREO}\alias{PREP}\alias{PREV}\alias{PROF}\alias{REC}
\alias{RED}\alias{RPHI}\alias{SEL}\alias{SENS}\alias{SPEC}\alias{SU}
\alias{TNR}\alias{TPR}\alias{TS}\alias{UC}\alias{VI2}\alias{WACC}
\alias{WLR}\alias{WPV}\alias{YJS}\alias{FAR}
\title{ Compute Performance Measures or Binary Classification }
\description{
  The function computes various performance measures for binary classification.
}
\usage{
perfMeasures(pred, pred.group, truth, namePos, cutoff = 0.5,
             weight = 0.5, wACC = weight, wLR = weight, 
             wPV = weight, beta = 1, measures = "all")
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
  \item{wACC}{ weight used for computing the weighted accuracy, where sensitivity 
  is multiplied by \code{wACC} and specificity by \code{1-wACC}. Must be in [0,1].}
  \item{wLR}{ weight used for computing the weighted likelihood ratio, where PLR 
  is multiplied by \code{wLR} and NLR by \code{1-wLR}. Must be in [0,1].}
  \item{wPV}{ weight used for computing the weighted predictive value, where PPV 
  is multiplied by \code{wPV} and NPV by \code{1-wPV}. Must be in [0,1].}
  \item{beta}{ beta coefficient used for computing the F beta score. Must be nonnegative.}
  \item{measures}{ character vector giving the measures that shall be computed;
  see details. Default \code{"all"} computes all measures available.}
}
\details{
  The function \code{perfMeasures} can be used to compute various performance 
  measures. For computing specific measures, the abbreviation given in 
  parentheses have to be specified in argument \code{measures}. Single measures
  can also be computed by respective functions, where their names are identical
  to the abbreviations given in the parentheses.
  
  The measures are:
  accuracy (ACC), probability of correct classification (PCC), 
  fraction correct (FC), simple matching coefficient (SMC), 
  Rand (similarity) index (RSI), probability of misclassification (PMC), 
  error rate (ER), fraction incorrect (FIC), 
  sensitivity (SENS), recall (REC), true positive rate (TPR), 
  probability of detection (PD), hit rate (HR), specificity (SPEC), 
  true negative rate (TNR), selectivity (SEL), detection rate (DR), 
  false positive rate (FPR), fall-out (FO), false alarm (rate) (FAR), 
  probability of false alarm (PFA), false negative rate (FNR), 
  miss rate (MR), false discovery rate (FDR), false omission rate (FOR), 
  prevalence (PREV), (positive) pre-test probability (PREP), 
  (positive) pre-test odds (PREO), detection prevalence (DPREV), 
  negative pre-test probability (NPREP), negative pre-test odds (NPREO), 
  no information rate (NIR), weighted accuracy (WACC), 
  balanced accuracy (BACC), (bookmaker) informedness (INF), 
  Youden's J statistic (YJS), deltap' (DPp), 
  positive likelihood ratio (PLR), negative likelihood ratio (NLR), 
  weighted likelihood ratio (WLR), balanced likelihood ratio (BLR), 
  diagnostic odds ratio (DOR), positive predictive value (PPV), 
  precision (PREC), (positive) post-test probability (POSTP), 
  (positive) post-test odds (POSTO), Bayes factor G1 (BFG1), 
  negative predictive value (NPV), negative post-test probability (NPOSTP), 
  negative post-test odds (NPOSTO), Bayes factor G0 (BFG0), 
  markedness (MARK), deltap (DP), weighted predictive value (WPV), 
  balanced predictive value (BPV), F1 score (F1S), 
  Dice similarity coefficient (DSC), F beta score (FBS), 
  Jaccard similarity coefficient (JSC), threat score (TS), 
  critical success index (CSI), Matthews' correlation coefficient (MCC), 
  Pearson's correlation (r phi) (RPHI), Phi coefficient (PHIC), 
  Cramer's V (CRV), proportion of positive predictions (PPP), 
  expected accuracy (EACC), Cohen's kappa coefficient (CKC), 
  mutual information in bits (MI2), joint entropy in bits (JE2), 
  variation of information in bits (VI2), Jaccard distance (JD), 
  information quality ratio (INFQR), uncertainty coefficient (UC), 
  entropy coefficient (EC), proficiency (metric) (PROF), 
  deficiency (metric) (DFM), redundancy (RED), 
  symmetric uncertainty (SU), normalized uncertainty (NU)
  
  These performance measures have in common that they require a dichotomization
  of the computed predictions (classification function). For measuring the performance 
  without dichotomization one can apply function \code{perfScores}.
  
  The prevalence is the prevalence given by the data. This often is not identical
  to the prevalence of the population. Hence, it might be better to compute
  PPV and NPV (and derived measures) by applying function \code{predValues},
  where one can specify the assumed prevalence. This holds in general for all
  measures that depend on the prevalence.
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
\seealso{
\code{\link{confMatrix}}, \code{\link{predValues}}, \code{\link{perfScores}}
}
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

## selecting a subset of measures
perfMeasures(pred, truth = infert$case, namePos = 1, 
             measures = c("SENS", "SPEC", "BACC", "YJS"))
}
\keyword{univar}
