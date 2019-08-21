perfMeasures <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                         weight = 0.5, wACC = weight, wPV = weight,
                         digits = 3){
  confmat <- confMatrix(pred = pred, pred.group = pred.group, 
                        truth = truth, namePos = namePos, 
                        cutoff = cutoff)
  stopifnot(length(weights) == 1)
  stopifnot(length(wACC) == 1)
  stopifnot(length(wPV) == 1)
  if(weight < 0 | weight > 1) stop("'weight' has to be in [0, 1]")
  if(wACC < 0 | wACC > 1) stop("'wACC' has to be in [0, 1]")
  if(wPV < 0 | wPV > 1) stop("'wPV' has to be in [0, 1]")

  TP <- confmat[1,1]
  FN <- confmat[2,1]
  FP <- confmat[1,2]
  TN <- confmat[2,2]
  ACC <- (TN + TP)/(TP + TN + FP + FN)
  PCC <- ACC
  PMC <- 1 - ACC
  ERATE <- PMC
  SENS <- TP/(TP + FN)
  SPEC <- TN/(TN + FP)
  PREV <- (TP + FN)/(TP + TN + FP + FN)
  NIR <- max(PREV, 1-PREV)
  BACC <- 0.5*SENS + 0.5*SPEC
  WACC <- wACC*SENS + (1-wACC)*SPEC
  INF <- SENS + SPEC - 1
  YOUDEN <- INF
  PLR <- SENS/(1-SPEC)
  NLR <- (1-SENS)/SPEC
  PPV <- TP/(TP + FP)
  NPV <- TN/(TN + FN)
  MARK <- PPV + NPV - 1
  WPV <- wPV*PPV + (1-wPV)*NPV
  BPV <- 0.5*PPV + 0.5*NPV
  F1 <- 2*PPV*SENS/(PPV + SENS)
  MCC <- sign(INF)*sqrt(INF*MARK)
  PP <- (TP + FP)/(TP + TN + FP + FN)
  EACC <- PREV*PP + (1 - PREV)*(1 - PP)
  COHEN <- (ACC - EACC)/(1-EACC)
  DR <- TP/(TP + TN + FP + FN)
  value <- c(ACC, PCC, PMC, ERATE, SENS, SPEC, PREV, NIR, WACC, BACC, INF, YOUDEN,
             PLR, NLR, PPV, NPV, MARK, WPV, BPV, F1, MCC, PP, EACC, COHEN, DR)
  measure <- c("accuracy (ACC)", "probabiliy of correct classification (PCC)",
               "probability of missclassification (PMC)", "error rate",
               "sensitivity", "specificity", "prevalence", "no information rate",
               "weighted accuracy (wACC)", "balanced accuracy (BACC)",
               "informedness", "Youden's J statistic", "positive likelihood ratio (PLR)",
               "negative likelihood ratio (NLR)", "positive predictive value (PPV)",
               "negative predictive value (NPV)", "markedness",
               "weighted predictive value", "balanced predictive value",
               "F1 score", "Matthews' correlation coefficient (MCC)",
               "proportion of positive predictions",
               "expected accuracy", "Cohen's kappa coefficient", "detection rate")
  data.frame(Measure = measure, Value = round(value, digits))
}
