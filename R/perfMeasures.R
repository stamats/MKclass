perfMeasures <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                         weight = 0.5, wACC = weight, wLR = weight, 
                         wPV = weight, beta = 1, digits = 3){
  confmat <- confMatrix(pred = pred, pred.group = pred.group, 
                        truth = truth, namePos = namePos, 
                        cutoff = cutoff)
  stopifnot(length(weights) == 1)
  stopifnot(length(wACC) == 1)
  stopifnot(length(wLR) == 1)
  stopifnot(length(wPV) == 1)
  stopifnot(length(beta) == 1)
  if(weight < 0 | weight > 1) stop("'weight' has to be in [0, 1]")
  if(wACC < 0 | wACC > 1) stop("'wACC' has to be in [0, 1]")
  if(wLR < 0 | wLR > 1) stop("'wLR' has to be in [0, 1]")
  if(wPV < 0 | wPV > 1) stop("'wPV' has to be in [0, 1]")
  if(beta < 0) stop("'beta' has to be nonnegative")

  TP <- confmat[1,1]
  FN <- confmat[2,1]
  FP <- confmat[1,2]
  TN <- confmat[2,2]
  ACC <- (TN + TP)/(TP + TN + FP + FN)
  PCC <- ACC
  PMC <- 1 - ACC
  ERATE <- PMC
  SENS <- TP/(TP + FN)
  REC <- SENS
  SPEC <- TN/(TN + FP)
  PREV <- (TP + FN)/(TP + TN + FP + FN)
  NIR <- max(PREV, 1-PREV)
  BACC <- 0.5*SENS + 0.5*SPEC
  WACC <- wACC*SENS + (1-wACC)*SPEC
  INF <- SENS + SPEC - 1
  YOUDEN <- INF
  PLR <- SENS/(1-SPEC)
  NLR <- (1-SENS)/SPEC
  WLR <- wLR*PLR + (1-wLR)*NLR
  BLR <- 0.5*PLR + 0.5*NLR
  PPV <- TP/(TP + FP)
  PREC <- PPV
  NPV <- TN/(TN + FN)
  MARK <- PPV + NPV - 1
  WPV <- wPV*PPV + (1-wPV)*NPV
  BPV <- 0.5*PPV + 0.5*NPV
  F1 <- 2*PPV*SENS/(PPV + SENS)
  DSC <- F1
  Fbeta <- (1+beta^2)*PPV*SENS/(beta^2*PPV + SENS)
  JACC <- TP/(TP + FP + FN)
  SMC <- ACC
  RI <- ACC
  MCC <- sign(INF)*sqrt(INF*MARK)
  PPP <- (TP + FP)/(TP + TN + FP + FN)
  EACC <- PREV*PP + (1 - PREV)*(1 - PP)
  COHEN <- (ACC - EACC)/(1-EACC)
  DR <- TP/(TP + TN + FP + FN)
  value <- c(ACC, PCC, PMC, ERATE, SENS, SPEC, REC, PREV, NIR, WACC, BACC, INF, 
             YOUDEN, PLR, NLR, WLR, BLR, PPV, NPV, WPV, BPV, PREC, MARK, 
             Fbeta, F1, DSC, JACC, SMC, RI, MCC, PPP, EACC, COHEN, DR)
  measure <- c("accuracy (ACC)", "probabiliy of correct classification (PCC)",
               "probability of misclassification (PMC)", "error rate",
               "sensitivity", "specificity", "recall", "prevalence", 
               "no information rate (NIR)",
               "weighted accuracy (wACC)", "balanced accuracy (BACC)",
               "(Bookmarker) informedness", "Youden's J statistic", 
               "positive likelihood ratio (PLR)", "negative likelihood ratio (NLR)", 
               "weighted likelihood ratio (WLR)", "balanced likelihood ratio (BLR)", 
               "positive predictive value (PPV)", "negative predictive value (NPV)", 
               "weighted predictive value (WPV)", "balanced predictive value (BPV)",
               "precision", "markedness",
               "Fbeta score", "F1 score", "Dice similarity coefficient (DSC)", 
               "Jaccard similarity coefficient (JSC)",
               "Simple matching coefficient (SMC)", "Rand index (RI)",
               "Matthews' correlation coefficient (MCC)",
               "proportion of positive predictions (PPP)",
               "expected accuracy (EACC)", 
               "Cohen's kappa coefficient (kappa)", 
               "detection rate")
  data.frame(Measure = measure, Value = round(value, digits))
}

ACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  acc <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[1,2]
  names(acc) <- "accuracy"
  acc
}
PCC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  pcc <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[2,2]
  names(pcc) <- "probability of correct classification"
  pcc
}
PMC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  pmc <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[3,2]
  names(pmc) <- "probability of misclassification"
  pmc
}
errorRate <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                      digits = 3){
  err <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[4,2]
  names(err) <- "error rate"
  err
}
sensitivity <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                        digits = 3){
  sens <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[5,2]
  names(sens) <- "sensitivity"
  sens
}
specificity <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                        digits = 3){
  spec <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                       namePos = namePos, cutoff = cutoff, digits = digits)[6,2]
  names(spec) <- "specificity"
  spec
}
recall <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                   digits = 3){
  rec <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[7,2]
  names(rec) <- "recall"
  rec
}
prevalence <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                        digits = 3){
  prev <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                       namePos = namePos, cutoff = cutoff, digits = digits)[8,2]
  names(prev) <- "prevalence"
  prev
}
NIR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  nir <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[9,2]
  names(nir) <- "no information rate"
  nir
}
WACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3, weight = 0.5){
  wacc <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                       namePos = namePos, cutoff = cutoff, digits = digits,
                       wACC = weight)[10,2]
  names(wacc) <- "weighted accuracy"
  wacc
} 
BACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  bacc <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                       namePos = namePos, cutoff = cutoff, digits = digits)[11,2]
  names(bacc) <- "balanced accuracy"
  bacc
} 
informedness <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                         digits = 3){
  inf <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[12,2]
  names(inf) <- "(Bookmarker) informedness"
  inf
}
YoudenJ <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                    digits = 3){
  you <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[13,2]
  names(you) <- "Youden's J statistic"
  you
}
PLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  plr <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[14,2]
  names(plr) <- "positive likelihood ratio"
  plr
}
NLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  nlr <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[15,2]
  names(nlr) <- "negative likelihood ratio"
  nlr
}
WLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3, weight = 0.5){
  wlr <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      wLR = weight)[16,2]
  names(wlr) <- "weighted likelihood ratio"
  wlr
}
BLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  blr <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[17,2]
  names(blr) <- "balanced likelihood ratio"
  blr
}
PPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  ppv <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[18,2]
  names(ppv) <- "positive predictive value"
  ppv
}
NPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  npv <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[19,2]
  names(npv) <- "negative predictive value"
  npv
}
WPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                       digits = 3, weight = 0.5){
  wpv <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      wPV = weight)[20,2]
  names(wpv) <- "weighted predicted value"
  wpv
}
BPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3, weight = 0.5){
  bpv <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      wPV = weight)[21,2]
  names(bpv) <- "balanced predicted value"
  bpv
}
precision <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                       digits = 3){
  prec <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                       namePos = namePos, cutoff = cutoff, digits = digits)[22,2]
  names(prec) <- "precision"
  prec
}
markedness <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                       digits = 3){
  mar <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[23,2]
  names(mar) <- "markedness"
  mar
}
Fbeta <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3, beta = 1){
  fb <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                     namePos = namePos, cutoff = cutoff, digits = digits,
                     beta = beta)[24,2]
  names(fb) <- "Fbeta score"
  fb
}
F1score <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                    digits = 3){
  f1 <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                     namePos = namePos, cutoff = cutoff, digits = digits)[25,2]
  names(f1) <- "F1 score"
  f1
}
DSC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  dsc <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                     namePos = namePos, cutoff = cutoff, digits = digits)[26,2]
  names(dsc) <- "Dice similarity coefficient"
  dsc
}
JSC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  jsc <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[27,2]
  names(jsc) <- "Jaccard similarity coefficient"
  jsc
}
SMC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  smc <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[28,2]
  names(smc) <- "Simple matching coefficient"
  smc
}
RI <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  ri <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[29,2]
  names(ri) <- "Rand index"
  ri
}
MCC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  mcc <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[30,2]
  names(mcc) <- "Matthews' correlation coefficient"
  mcc
}
PPP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  ppp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits)[31,2]
  names(ppp) <- "proportion of positive predictions"
  ppp
}
EACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  eacc <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                       namePos = namePos, cutoff = cutoff, digits = digits)[32,2]
  names(eacc) <- "expected accuracy"
  eacc
}
kappa <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  k <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                    namePos = namePos, cutoff = cutoff, digits = digits)[33,2]
  names(k) <- "Cohen's kappa coefficient"
  k
}
detectionRate <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                          digits = 3){
  dr <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                     namePos = namePos, cutoff = cutoff, digits = digits)[34,2]
  names(dr) <- "detection rate"
  dr
}
