perfMeasures <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                         weight = 0.5, wACC = weight, wLR = weight, 
                         wPV = weight, beta = 1, measures = "all"){
  stopifnot(length(weight) == 1)
  if(weight < 0 | weight > 1) stop("'weight' has to be in [0, 1]")
  
  confmat <- confMatrix(pred = pred, pred.group = pred.group, 
                        truth = truth, namePos = namePos, 
                        cutoff = cutoff, relative = FALSE)
  measureNames <- NULL
  measureValues <- NULL
  if(any(measures == "all")){
    measures <- c("ACC", "PCC", "FC", "SMC", "RSI", "PMC", "ER", "FIC",
                  "SENS", "REC", "TPR", "PD", "HR", "SPEC", "TNR", "SEL", 
                  "DR", "FPR", "FO", "FAR", "PFA", "FNR", "MR", "FDR", "FOR", 
                  "PREV", "PREP", "PREO", "DPREV", "NPREP", "NPREO", "NIR", 
                  "WACC", "BACC", "INF", "YJS", "DPp", 
                  "PLR", "NLR", "WLR", "BLR", "DOR", 
                  "PPV", "PREC", "POSTP", "POSTO", "BFG1", 
                  "NPV", "NPOSTP", "NPOSTO", "BFG0",
                  "MARK", "DP", "WPV", "BPV", "F1S", "DSC", 
                  "FBS", "JSC", "TS", "CSI", 
                  "MCC", "RPHI", "PHIC", "CRV", "PPP", "EACC", "CKC", 
                  "MI2", "JE2", "VI2", "JD", "INFQR", 
                  "UC", "EC", "PROF", "DFM",
                  "RED", "SU", "NU")
  }
  TP <- confmat[1,1]
  FN <- confmat[2,1]
  FP <- confmat[1,2]
  TN <- confmat[2,2]
  if("ACC" %in% measures){ 
    ACC <- (TN + TP)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "accuracy (ACC)")
    measureValues <- c(measureValues, ACC)
  }
  if("PCC" %in% measures){ 
    PCC <- (TN + TP)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "probability of correct classification (PCC)")
    measureValues <- c(measureValues, PCC)
  }
  if("FC" %in% measures){ 
    FC <- (TN + TP)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "fraction correct (FC)")
    measureValues <- c(measureValues, FC)
  }
  if("SMC" %in% measures){ 
    SMC <- (TN + TP)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "simple matching coefficient (SMC)")
    measureValues <- c(measureValues, SMC)
  }
  if("RSI" %in% measures){ 
    RSI <- (TN + TP)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "Rand (similarity) index (RSI)")
    measureValues <- c(measureValues, RSI)
  }
  if("PMC" %in% measures){ 
    PMC <- 1 - (TN + TP)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "probability of misclassification (PMC)")
    measureValues <- c(measureValues, PMC)
  }
  if("ER" %in% measures){ 
    ER <- 1 - (TN + TP)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "error rate (ER)")
    measureValues <- c(measureValues, ER)
  }
  if("FIC" %in% measures){ 
    FIC <- 1 - (TN + TP)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "fraction incorrect (FIC)")
    measureValues <- c(measureValues, FIC)
  }
  if("SENS" %in% measures){ 
    SENS <- TP/(TP + FN)
    measureNames <- c(measureNames, "sensitivity (SENS)")
    measureValues <- c(measureValues, SENS)
  }
  if("REC" %in% measures){ 
    REC <- TP/(TP + FN)
    measureNames <- c(measureNames, "recall (REC)")
    measureValues <- c(measureValues, REC)
  }
  if("TPR" %in% measures){ 
    TPR <- TP/(TP + FN)
    measureNames <- c(measureNames, "true positive rate (TPR)")
    measureValues <- c(measureValues, TPR)
  }
  if("PD" %in% measures){ 
    PD <- TP/(TP + FN)
    measureNames <- c(measureNames, "probability of detection (PD)")
    measureValues <- c(measureValues, PD)
  }
  if("HR" %in% measures){ 
    HR <- TP/(TP + FN)
    measureNames <- c(measureNames, "hit rate (HR)")
    measureValues <- c(measureValues, HR)
  }
  if("SPEC" %in% measures){ 
    SPEC <- TN/(TN + FP)
    measureNames <- c(measureNames, "specificity (SPEC)")
    measureValues <- c(measureValues, SPEC)
  }
  if("TNR" %in% measures){ 
    TNR <- TN/(TN + FP)
    measureNames <- c(measureNames, "true negative rate (TNR)")
    measureValues <- c(measureValues, TNR)
  }
  if("SEL" %in% measures){ 
    SEL <- TN/(TN + FP)
    measureNames <- c(measureNames, "selectivity (SEL)")
    measureValues <- c(measureValues, SEL)
  }
  if("DR" %in% measures){ 
    DR <- TP/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "detection rate (DR)")
    measureValues <- c(measureValues, DR)
  }
  if("FPR" %in% measures){ 
    FPR <- FP/(TN+FP)
    measureNames <- c(measureNames, "false positive rate (FPR)")
    measureValues <- c(measureValues, FPR)
  }
  if("FO" %in% measures){ 
    FO <- FP/(TN+FP)
    measureNames <- c(measureNames, "fall-out (FO)")
    measureValues <- c(measureValues, FO)
  }
  if("FAR" %in% measures){ 
    FAR <- FP/(TN+FP)
    measureNames <- c(measureNames, "false alarm (rate) (FAR)")
    measureValues <- c(measureValues, FAR)
  }
  if("PFA" %in% measures){ 
    PFA <- FP/(TN+FP)
    measureNames <- c(measureNames, "probability of false alarm (PFA)")
    measureValues <- c(measureValues, PFA)
  }
  if("FNR" %in% measures){ 
    FNR <- FN/(TP+FN)
    measureNames <- c(measureNames, "false negative rate (FNR)")
    measureValues <- c(measureValues, FNR)
  }
  if("MR" %in% measures){ 
    MR <- FN/(TP+FN)
    measureNames <- c(measureNames, "miss rate (MR)")
    measureValues <- c(measureValues, MR)
  }
  if("FDR" %in% measures){ 
    FDR <- FP/(TP+FP)
    measureNames <- c(measureNames, "false discovery rate (FDR)")
    measureValues <- c(measureValues, FDR)
  }
  if("FOR" %in% measures){ 
    FOR <- FN/(TN+FN)
    measureNames <- c(measureNames, "false omission rate (FOR)")
    measureValues <- c(measureValues, FOR)
  }
  if("PREV" %in% measures){ 
    PREV <- (TP + FN)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "prevalence (PREV)")
    measureValues <- c(measureValues, PREV)
  }
  if("PREP" %in% measures){ 
    PREP <- (TP + FN)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "(positive) pre-test probability (PREP)")
    measureValues <- c(measureValues, PREP)
  }
  if("PREO" %in% measures){ 
    PREP <- (TP + FN)/(TP + TN + FP + FN)
    PREO <- PREP/(1-PREP)
    measureNames <- c(measureNames, "(positive) pre-test odds (PREO)")
    measureValues <- c(measureValues, PREO)
  }
  if("DPREV" %in% measures){ 
    DPREV <- (TP+FP)/(TP+FP+TN+FN)
    measureNames <- c(measureNames, "detection prevalence (DPREV)")
    measureValues <- c(measureValues, DPREV)
  }
  if("NPREP" %in% measures){ 
    NPREP <- (TN + FP)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "negative pre-test probability (NPREP)")
    measureValues <- c(measureValues, NPREP)
  }
  if("NPREO" %in% measures){ 
    NPREP <- (TN + FP)/(TP + TN + FP + FN)
    NPREO <- NPREP/(1-NPREP)
    measureNames <- c(measureNames, "negative pre-test odds (NPREO)")
    measureValues <- c(measureValues, NPREO)
  }
  if("NIR" %in% measures){ 
    PREV <- (TP + FN)/(TP + TN + FP + FN)
    NIR <- max(PREV, 1-PREV)
    measureNames <- c(measureNames, "no information rate (NIR)")
    measureValues <- c(measureValues, NIR)
  }
  if("WACC" %in% measures){ 
    stopifnot(length(wACC) == 1)
    if(wACC < 0 | wACC > 1) stop("'wACC' has to be in [0, 1]")
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    WACC <- wACC*SENS + (1-wACC)*SPEC
    BACC <- 0.5*SENS + 0.5*SPEC
    measureNames <- c(measureNames, "weighted accuracy (WACC)")
    measureValues <- c(measureValues, WACC)
  }
  if("BACC" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    BACC <- 0.5*SENS + 0.5*SPEC
    measureNames <- c(measureNames, "balanced accuracy (BACC)")
    measureValues <- c(measureValues, BACC)
  }
  if("INF" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    INF <- SENS + SPEC - 1
    measureNames <- c(measureNames, "(bookmaker) informedness (INF)")
    measureValues <- c(measureValues, INF)
  }
  if("YJS" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    YJS <- SENS + SPEC - 1
    measureNames <- c(measureNames, "Youden's J statistic (YJS)")
    measureValues <- c(measureValues, YJS)
  }
  if("DPp" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    DPp <- SENS + SPEC - 1
    measureNames <- c(measureNames, "deltap' (DPp)")
    measureValues <- c(measureValues, DPp)
  }
  if("PLR" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    PLR <- SENS/(1-SPEC)
    measureNames <- c(measureNames, "positive likelihood ratio (PLR)")
    measureValues <- c(measureValues, PLR)
  }
  if("NLR" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    NLR <- (1-SENS)/SPEC
    measureNames <- c(measureNames, "negative likelihood ratio (NLR)")
    measureValues <- c(measureValues, NLR)
  }
  if("WLR" %in% measures){ 
    stopifnot(length(wLR) == 1)
    if(wLR < 0 | wLR > 1) stop("'wLR' has to be in [0, 1]")
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    PLR <- SENS/(1-SPEC)
    NLR <- (1-SENS)/SPEC
    WLR <- wLR*PLR + (1-wLR)*NLR
    measureNames <- c(measureNames, "weighted likelihood ratio (WLR)")
    measureValues <- c(measureValues, WLR)
  }
  if("BLR" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    PLR <- SENS/(1-SPEC)
    NLR <- (1-SENS)/SPEC
    BLR <- 0.5*PLR + 0.5*NLR
    measureNames <- c(measureNames, "balanced likelihood ratio (BLR)")
    measureValues <- c(measureValues, BLR)
  }
  if("DOR" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    PLR <- SENS/(1-SPEC)
    NLR <- (1-SENS)/SPEC
    DOR <- PLR/NLR
    measureNames <- c(measureNames, "diagnostic odds ratio (DOR)")
    measureValues <- c(measureValues, DOR)
  }
  if("PPV" %in% measures){ 
    PPV <- TP/(TP + FP)
    measureNames <- c(measureNames, "positive predictive value (PPV)")
    measureValues <- c(measureValues, PPV)
  }
  if("PREC" %in% measures){ 
    PREC <- TP/(TP + FP)
    measureNames <- c(measureNames, "precision (PREC)")
    measureValues <- c(measureValues, PREC)
  }
  if("POSTP" %in% measures){ 
    POSTP <- TP/(TP + FP)
    measureNames <- c(measureNames, "(positive) post-test probability (POSTP)")
    measureValues <- c(measureValues, POSTP)
  }
  if("POSTO" %in% measures){ 
    POSTP <- TP/(TP + FP)
    POSTO <- POSTP/(1-POSTP)
    measureNames <- c(measureNames, "(positive) post-test odds (POSTO)")
    measureValues <- c(measureValues, POSTO)
  }
  if("BFG1" %in% measures){ 
    PREP <- (TP + FN)/(TP + TN + FP + FN)
    PREO <- PREP/(1-PREP)
    POSTP <- TP/(TP + FP)
    POSTO <- POSTP/(1-POSTP)
    BFG1 <- POSTO/PREO
    measureNames <- c(measureNames, "Bayes factor G1 (BFG1)")
    measureValues <- c(measureValues, BFG1)
  }
  if("NPV" %in% measures){ 
    NPV <- TN/(TN + FN)
    measureNames <- c(measureNames, "negative predictive value (NPV)")
    measureValues <- c(measureValues, NPV)
  }
  if("NPOSTP" %in% measures){ 
    NPOSTP <- TN/(TN + FN)
    measureNames <- c(measureNames, "negative post-test probability (NPOSTP)")
    measureValues <- c(measureValues, NPOSTP)
  }
  if("NPOSTO" %in% measures){ 
    NPOSTP <- TN/(TN + FN)
    NPOSTO <- NPOSTP/(1-NPOSTP)
    measureNames <- c(measureNames, "negative post-test odds (NPOSTO)")
    measureValues <- c(measureValues, NPOSTO)
  }
  if("BFG0" %in% measures){ 
    NPREP <- (TN + FP)/(TP + TN + FP + FN)
    NPREO <- NPREP/(1-NPREP)
    NPOSTP <- TN/(TN + FN)
    NPOSTO <- NPOSTP/(1-NPOSTP)
    BFG0 <- NPOSTO/NPREO
    measureNames <- c(measureNames, "Bayes factor G0 (BFG0)")
    measureValues <- c(measureValues, BFG0)
  }
  if("MARK" %in% measures){ 
    PPV <- TP/(TP + FP)
    NPV <- TN/(TN + FN)
    MARK <- PPV + NPV - 1
    measureNames <- c(measureNames, "markedness (MARK)")
    measureValues <- c(measureValues, MARK)
  }
  if("DP" %in% measures){ 
    PPV <- TP/(TP + FP)
    NPV <- TN/(TN + FN)
    DP <- PPV + NPV - 1
    measureNames <- c(measureNames, "deltap (DP)")
    measureValues <- c(measureValues, DP)
  }
  if("WPV" %in% measures){ 
    stopifnot(length(wPV) == 1)
    if(wPV < 0 | wPV > 1) stop("'wPV' has to be in [0, 1]")
    PPV <- TP/(TP + FP)
    NPV <- TN/(TN + FN)
    WPV <- wPV*PPV + (1-wPV)*NPV
    measureNames <- c(measureNames, "weighted predictive value (WPV)")
    measureValues <- c(measureValues, WPV)
  }
  if("BPV" %in% measures){ 
    PPV <- TP/(TP + FP)
    NPV <- TN/(TN + FN)
    BPV <- 0.5*PPV + 0.5*NPV
    measureNames <- c(measureNames, "balanced predictive value (BPV)")
    measureValues <- c(measureValues, BPV)
  }
  if("F1S" %in% measures){ 
    PPV <- TP/(TP + FP)
    SENS <- TP/(TP + FN)
    F1S <- 2*PPV*SENS/(PPV + SENS)
    measureNames <- c(measureNames, "F1 score (F1S)")
    measureValues <- c(measureValues, F1S)
  }
  if("DSC" %in% measures){ 
    PPV <- TP/(TP + FP)
    SENS <- TP/(TP + FN)
    DSC <- 2*PPV*SENS/(PPV + SENS)
    measureNames <- c(measureNames, "Dice similarity coefficient (DSC)")
    measureValues <- c(measureValues, DSC)
  }
  if("FBS" %in% measures){ 
    stopifnot(length(beta) == 1)
    if(beta < 0) stop("'beta' has to be nonnegative")
    PPV <- TP/(TP + FP)
    SENS <- TP/(TP + FN)
    FBS <- (1+beta^2)*PPV*SENS/(beta^2*PPV + SENS)
    measureNames <- c(measureNames, "F beta score (FBS)")
    measureValues <- c(measureValues, FBS)
  }
  if("JSC" %in% measures){ 
    JSC <- TP/(TP + FP + FN)
    measureNames <- c(measureNames, "Jaccard similarity coefficient (JSC)")
    measureValues <- c(measureValues, JSC)
  }
  if("TS" %in% measures){ 
    TS <- TP/(TP + FP + FN)
    measureNames <- c(measureNames, "threat score (TS)")
    measureValues <- c(measureValues, TS)
  }
  if("CSI" %in% measures){ 
    CSI <- TP/(TP + FP + FN)
    measureNames <- c(measureNames, "critical success index (CSI)")
    measureValues <- c(measureValues, CSI)
  }
  if("MCC" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    INF <- SENS + SPEC - 1
    PPV <- TP/(TP + FP)
    NPV <- TN/(TN + FN)
    MARK <- PPV + NPV - 1
    MCC <- sign(INF)*sqrt(INF*MARK)
    measureNames <- c(measureNames, "Matthews' correlation coefficient (MCC)")
    measureValues <- c(measureValues, MCC)
  }
  if("RPHI" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    INF <- SENS + SPEC - 1
    PPV <- TP/(TP + FP)
    NPV <- TN/(TN + FN)
    MARK <- PPV + NPV - 1
    RPHI <- sign(INF)*sqrt(INF*MARK)
    measureNames <- c(measureNames, "Pearson's correlation (r phi) (RPHI)")
    measureValues <- c(measureValues, RPHI)
  }
  if("PHIC" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    INF <- SENS + SPEC - 1
    PPV <- TP/(TP + FP)
    NPV <- TN/(TN + FN)
    MARK <- PPV + NPV - 1
    PHIC <- sign(INF)*sqrt(INF*MARK)
    measureNames <- c(measureNames, "Phi coefficient (PHIC)")
    measureValues <- c(measureValues, PHIC)
  }
  if("CRV" %in% measures){ 
    SENS <- TP/(TP + FN)
    SPEC <- TN/(TN + FP)
    INF <- SENS + SPEC - 1
    PPV <- TP/(TP + FP)
    NPV <- TN/(TN + FN)
    MARK <- PPV + NPV - 1
    CRV <- sign(INF)*sqrt(INF*MARK)
    measureNames <- c(measureNames, "Cramer's V (CRV)")
    measureValues <- c(measureValues, PHIC)
  }
  if("PPP" %in% measures){ 
    PPP <- (TP + FP)/(TP + TN + FP + FN)
    measureNames <- c(measureNames, "proportion of positive predictions (PPP)")
    measureValues <- c(measureValues, PPP)
  }
  if("EACC" %in% measures){ 
    PREV <- (TP + FN)/(TP + TN + FP + FN)
    PPP <- (TP + FP)/(TP + TN + FP + FN)
    EACC <- PREV*PPP + (1 - PREV)*(1 - PPP)
    measureNames <- c(measureNames, "expected accuracy (EACC)")
    measureValues <- c(measureValues, EACC)
  }
  if("CKC" %in% measures){ 
    PREV <- (TP + FN)/(TP + TN + FP + FN)
    PPP <- (TP + FP)/(TP + TN + FP + FN)
    EACC <- PREV*PPP + (1 - PREV)*(1 - PPP)
    ACC <- (TN + TP)/(TP + TN + FP + FN)
    CKC <- (ACC - EACC)/(1-EACC)
    measureNames <- c(measureNames, "Cohen's kappa coefficient (CKC)")
    measureValues <- c(measureValues, CKC)
  }
  if("MI2" %in% measures){ 
    ## https://en.wikipedia.org/wiki/Mutual_information
    P <- TP + FN
    N <- TN + FP
    LTP <- TP*log2(TP/(P*(TP+FP)))
    LFP <- FP*log2(FP/(N*(FP+TP)))
    LFN <- FN*log2(FN/(P*(FN+TN)))
    LTN <- TN*log2(TN/(N*(TN+FN)))
    MI2 <-  log2(P+N) + (LTN + LTP + LFP + LFN)/(P + N)
    measureNames <- c(measureNames, "mutual information in bits (MI2)")
    measureValues <- c(measureValues, MI2)
  }
  if("JE2" %in% measures){ 
    ## https://en.wikipedia.org/wiki/Joint_entropy
    P <- TP + FN
    N <- TN + FP
    L2TP <- TP*log(TP)
    L2FP <- FP*log(FP)
    L2FN <- FN*log(FN)
    L2TN <- TN*log(TN)
    JE2 <- log(P+N) - (L2TN + L2TP + L2FP + L2FN)/(P + N)
    measureNames <- c(measureNames, "joint entropy in bits (JE2)")
    measureValues <- c(measureValues, JE2)
  }
  if("VI2" %in% measures){ 
    ## https://en.wikipedia.org/wiki/Variation_of_information
    P <- TP + FN
    N <- TN + FP
    LTP <- TP*log2(TP/(P*(TP+FP)))
    LFP <- FP*log2(FP/(N*(FP+TP)))
    LFN <- FN*log2(FN/(P*(FN+TN)))
    LTN <- TN*log2(TN/(N*(TN+FN)))
    MI2 <-  log2(P+N) + (LTN + LTP + LFP + LFN)/(P + N)
    L2TP <- TP*log2(TP)
    L2FP <- FP*log2(FP)
    L2FN <- FN*log2(FN)
    L2TN <- TN*log2(TN)
    JE2 <- log2(P+N) - (L2TN + L2TP + L2FP + L2FN)/(P + N)
    VI2 <- JE2 - MI2
    measureNames <- c(measureNames, "variation of information in bits (VI2)")
    measureValues <- c(measureValues, VI2)
  }
  if("JD" %in% measures){ 
    ## https://en.wikipedia.org/wiki/Mutual_information
    P <- TP + FN
    N <- TN + FP
    LTP <- TP*log2(TP/(P*(TP+FP)))
    LFP <- FP*log2(FP/(N*(FP+TP)))
    LFN <- FN*log2(FN/(P*(FN+TN)))
    LTN <- TN*log2(TN/(N*(TN+FN)))
    MI2 <-  log2(P+N) + (LTN + LTP + LFP + LFN)/(P + N)
    L2TP <- TP*log2(TP)
    L2FP <- FP*log2(FP)
    L2FN <- FN*log2(FN)
    L2TN <- TN*log2(TN)
    JE2 <- log2(P+N) - (L2TN + L2TP + L2FP + L2FN)/(P + N)
    JD <- 1 - MI2/JE2
    measureNames <- c(measureNames, "Jaccard distance (JD)")
    measureValues <- c(measureValues, JD)
  }
  if("INFQR" %in% measures){ 
    ## https://en.wikipedia.org/wiki/Mutual_information
    P <- TP + FN
    N <- TN + FP
    LTP <- TP*log2(TP/(P*(TP+FP)))
    LFP <- FP*log2(FP/(N*(FP+TP)))
    LFN <- FN*log2(FN/(P*(FN+TN)))
    LTN <- TN*log2(TN/(N*(TN+FN)))
    MI2 <-  log2(P+N) + (LTN + LTP + LFP + LFN)/(P + N)
    L2TP <- TP*log2(TP)
    L2FP <- FP*log2(FP)
    L2FN <- FN*log2(FN)
    L2TN <- TN*log2(TN)
    JE2 <- log2(P+N) - (L2TN + L2TP + L2FP + L2FN)/(P + N)
    INFQR <- MI2/JE2
    measureNames <- c(measureNames, "information quality ratio (INFQR)")
    measureValues <- c(measureValues, INFQR)
  }
  if("UC" %in% measures){ 
    ## https://en.wikipedia.org/wiki/Evaluation_of_binary_classifiers
    P <- TP + FN
    N <- TN + FP
    LTP <- TP*log(TP/(P*(TP+FP)))
    LFP <- FP*log(FP/(N*(FP+TP)))
    LFN <- FN*log(FN/(P*(FN+TN)))
    LTN <- TN*log(TN/(N*(TN+FN)))
    L <- (P + N)*log(P + N)
    ## Error in wikipedia, wrong denominator!
    LP <- P*log(P)
    LN <- N*log(N)
    UC <- (L+LTP+LFP+LFN+LTN)/(L-LP-LN)
    measureNames <- c(measureNames, "uncertainty coefficient (UC)")
    measureValues <- c(measureValues, UC)
  }
  if("EC" %in% measures){ 
    P <- TP + FN
    N <- TN + FP
    L <- (P + N)*log(P + N)
    LTP <- TP*log(TP/((TP+FP)*(TP+FN)))
    LFP <- FP*log(FP/((FP+TP)*(FP+TN)))
    LFN <- FN*log(FN/((FN+TP)*(FN+TN)))
    LTN <- TN*log(TN/((TN+FP)*(TN+FN)))
    LP <- P*log(P)
    LN <- N*log(N)
    EC <- (L+LTP+LFP+LFN+LTN)/(L-LP-LN)
    measureNames <- c(measureNames, "entropy coefficient (EC)")
    measureValues <- c(measureValues, EC)
  }
  if("PROF" %in% measures){ 
    P <- TP + FN
    N <- TN + FP
    L <- (P + N)*log(P + N)
    LTP <- TP*log(TP/((TP+FP)*(TP+FN)))
    LFP <- FP*log(FP/((FP+TP)*(FP+TN)))
    LFN <- FN*log(FN/((FN+TP)*(FN+TN)))
    LTN <- TN*log(TN/((TN+FP)*(TN+FN)))
    LP <- P*log(P)
    LN <- N*log(N)
    PROF <- (L+LTP+LFP+LFN+LTN)/(L-LP-LN)
    measureNames <- c(measureNames, "proficiency (metric) (PROF)")
    measureValues <- c(measureValues, PROF)
  }
  if("DFM" %in% measures){ 
    P <- TP + FN
    N <- TN + FP
    L <- (P + N)*log(P + N)
    LTP <- TP*log(TP/((TP+FP)*(TP+FN)))
    LFP <- FP*log(FP/((FP+TP)*(FP+TN)))
    LFN <- FN*log(FN/((FN+TP)*(FN+TN)))
    LTN <- TN*log(TN/((TN+FP)*(TN+FN)))
    LP <- P*log(P)
    LN <- N*log(N)
    PROF <- (L+LTP+LFP+LFN+LTN)/(L-LP-LN)
    DFM <- 1 - PROF
    measureNames <- c(measureNames, "deficiency (metric) (DFM)")
    measureValues <- c(measureValues, DFM)
  }
  if("RED" %in% measures){ 
    ## https://en.wikipedia.org/wiki/Mutual_information
    P <- TP + FN
    N <- TN + FP
    LTP <- TP*log2(TP/(P*(TP+FP)))
    LFP <- FP*log2(FP/(N*(FP+TP)))
    LFN <- FN*log2(FN/(P*(FN+TN)))
    LTN <- TN*log2(TN/(N*(TN+FN)))
    MI2 <-  log2(P+N) + (LTN + LTP + LFP + LFN)/(P + N)
    HX <- log2(P+N) - (N*log2(N) + P*log2(P))/(P+N)
    HY <- log2(P+N) - ((TN+FN)*log2(TN+FN) + (TP+FP)*log2(TP+FP))/(P+N)
    RED <- MI2/(HX+HY)
    measureNames <- c(measureNames, "redundancy (RED)")
    measureValues <- c(measureValues, RED)
  }
  if("SU" %in% measures){ 
    ## https://en.wikipedia.org/wiki/Mutual_information
    P <- TP + FN
    N <- TN + FP
    LTP <- TP*log2(TP/(P*(TP+FP)))
    LFP <- FP*log2(FP/(N*(FP+TP)))
    LFN <- FN*log2(FN/(P*(FN+TN)))
    LTN <- TN*log2(TN/(N*(TN+FN)))
    MI2 <-  log2(P+N) + (LTN + LTP + LFP + LFN)/(P + N)
    HX <- log2(P+N) - (N*log2(N) + P*log2(P))/(P+N)
    HY <- log2(P+N) - ((TN+FN)*log2(TN+FN) + (TP+FP)*log2(TP+FP))/(P+N)
    SU <- 2*MI2/(HX+HY)
    measureNames <- c(measureNames, "symmetric uncertainty (SU)")
    measureValues <- c(measureValues, SU)
  }
  if("NU" %in% measures){ 
    ## https://en.wikipedia.org/wiki/Mutual_information
    P <- TP + FN
    N <- TN + FP
    LTP <- TP*log2(TP/(P*(TP+FP)))
    LFP <- FP*log2(FP/(N*(FP+TP)))
    LFN <- FN*log2(FN/(P*(FN+TN)))
    LTN <- TN*log2(TN/(N*(TN+FN)))
    MI2 <-  log2(P+N) + (LTN + LTP + LFP + LFN)/(P + N)
    HX <- log2(P+N) - (N*log2(N) + P*log2(P))/(P+N)
    HY <- log2(P+N) - ((TN+FN)*log2(TN+FN) + (TP+FP)*log2(TP+FP))/(P+N)
    NU <- MI2/sqrt(HX*HY)
    measureNames <- c(measureNames, "normalized uncertainty (NU)")
    measureValues <- c(measureValues, NU)
  }
  
  res <- list(measure = measureNames, value = measureValues)
  class(res) <- "perfMeasure"
  res
}
print.perfMeasure <- function(x, digits = getOption("digits"), prefix = "\t\t", ...){
  cat("\n")
  cat(strwrap("Performance Measure(s)", prefix = prefix), sep = "\n")
  cat("\n")
  print(data.frame(Measure = x$measure, Value = x$value))
  invisible(x)
}

ACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "ACC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PCC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PCC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
FC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "FC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
SMC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "SMC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
RSI <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "RSI")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PMC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PMC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
ER <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "ER")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
FIC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "FIC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
SENS <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "SENS")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
REC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "REC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
TPR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "TPR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PD <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PD")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
HR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "HR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
SPEC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "SPEC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
TNR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "TNR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
SEL <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "SEL")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
DR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "DR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
FPR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "FPR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
FO <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "FO")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
FAR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "FAR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PFA <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PFA")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
FNR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "FNR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
MR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "MR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
FDR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "FDR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
FOR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "FOR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PREV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PREV")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PREP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PREP")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PREO <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PREO")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
DPREV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "DPREV")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
NPREP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "NPREP")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
NPREO <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "NPREO")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
NIR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "NIR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
WACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5, wACC = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "WACC", wACC = wACC)
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
BACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "BACC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
INF <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "INF")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
YJS <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "YJS")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
DPp <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "DPp")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PLR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
NLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "NLR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
WLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5, wLR = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "WLR", wLR = wLR)
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
BLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "BLR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
DOR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "DOR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PPV")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PREC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PREC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
POSTP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "POSTP")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
POSTO <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "POSTO")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
BFG1 <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "BFG1")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
NPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "NPV")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
NPOSTP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "NPOSTP")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
NPOSTO <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "NPOSTO")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
BFG0 <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "BFG0")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
MARK <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "MARK")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
DP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "DP")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
WPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5, wPV = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "WPV", wPV = wPV)
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
BPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "BPV")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
F1S <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "F1S")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
DSC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "DSC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
FBS <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3, beta = 1){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "FBS", beta = beta)
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
JSC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "JSC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
TS <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "TS")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
CSI <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "CSI")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
MCC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "MCC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
RPHI <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "RPHI")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PHIC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PHIC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
CRV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "CRV")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PPP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PPP")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
EACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "EACC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
CKC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "CKC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
MI2 <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "MI2")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
JE2 <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "JE2")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
VI2 <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "VI2")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
JD <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "JD")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
INFQR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "INFQR")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
UC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "UC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
EC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "EC")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
PROF <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "PROF")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
DFM <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "DFM")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
RED <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "RED")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
SU <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
              digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "SU")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
NU <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff,
                      measures = "NU")
  res <- tmp$value
  names(res) <- tmp$measure
  res
}
