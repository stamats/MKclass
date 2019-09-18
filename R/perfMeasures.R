perfMeasures <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                         weight = 0.5, wACC = weight, wLR = weight, 
                         wPV = weight, beta = 1, digits = 3, 
                         measures = "all"){
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
  
  data.frame(Measure = measureNames, Value = round(measureValues, digits))
}

ACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "ACC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PCC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PCC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
FC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "FC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
SMC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "SMC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
RSI <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "RSI")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PMC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PMC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
ER <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "ER")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
FIC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "FIC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
SENS <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "SENS")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
REC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "REC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
TPR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "TPR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PD <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PD")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
HR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "HR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
SPEC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "SPEC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
TNR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "TNR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
SEL <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "SEL")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
DR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "DR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
FPR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "FPR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
FO <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "FO")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
FAR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "FAR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PFA <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PFA")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
FNR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "FNR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
MR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "MR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
FDR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "FDR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
FOR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "FOR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PREV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PREV")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PREP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PREP")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PREO <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PREO")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
DPREV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "DPREV")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
NPREP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "NPREP")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
NPREO <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "NPREO")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
NIR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "NIR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
WACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5, wACC = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "WACC", wACC = wACC)
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
BACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "BACC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
INF <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "INF")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
YJS <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "YJS")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
DPp <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "DPp")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PLR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
NLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "NLR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
WLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5, wLR = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "WLR", wLR = wLR)
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
BLR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "BLR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
DOR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "DOR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PPV")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PREC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PREC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
POSTP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "POSTP")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
POSTO <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "POSTO")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
BFG1 <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "BFG1")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
NPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "NPV")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
NPOSTP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "NPOSTP")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
NPOSTO <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "NPOSTO")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
BFG0 <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "BFG0")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
MARK <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "MARK")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
DP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "DP")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
WPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5, wPV = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "WPV", wPV = wPV)
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
BPV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "BPV")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
F1S <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "F1S")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
DSC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "DSC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
FBS <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "FBS")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
JSC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "JSC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
TS <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "TS")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
CSI <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "CSI")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
MCC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "MCC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
RPHI <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "RPHI")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PHIC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PHIC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
CRV <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "CRV")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PPP <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PPP")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
EACC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "EACC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
CKC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "CKC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
MI2 <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "MI2")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
JE2 <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "JE2")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
VI2 <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "VI2")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
JD <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "JD")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
INFQR <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                  digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "INFQR")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
UC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "UC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
EC <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "EC")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PROF <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "PROF")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
DFM <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                 digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "DFM")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
RED <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "RED")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
SU <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
              digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "SU")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
NU <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
               digits = 3){
  tmp <- perfMeasures(pred = pred, pred.group = pred.group, truth = truth,
                      namePos = namePos, cutoff = cutoff, digits = digits,
                      measures = "NU")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
