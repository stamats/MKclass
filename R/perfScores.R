perfScores <- function(pred, truth, namePos, wBS = 0.5, digits = 3, 
                       scores = "all"){
  stopifnot(length(pred) == length(truth))
  stopifnot(is.numeric(pred))

  if(!is.factor(truth)) truth <- factor(truth)
  stopifnot(nlevels(truth) == 2)
  
  if(!is.character(namePos)) namePos <- as.character(namePos)
  stopifnot(namePos %in% levels(truth))
  
  if(any(pred > 1) | any(pred < 0)){
    pred <- exp(pred)/(1 + exp(pred))
  }
  
  scoreNames <- NULL
  scoreValues <- NULL
  if(any(scores == "all")){
    scores <- c("AUC", "GINI", "BS", "PBS", "NBS", "WBS", "BBS")
  }
  
  if("AUC" %in% scores){ 
    AUC <- AUC(pred, group = as.integer(truth == namePos))
    scoreNames <- c(scoreNames, "area under curve (AUC)")
    scoreValues <- c(scoreValues, AUC)
  }
  if("GINI" %in% scores){ 
    AUC <- AUC(pred, group = as.integer(truth == namePos))
    GINI <- 2*AUC - 1
    scoreNames <- c(scoreNames, "Gini index (GINI)")
    scoreValues <- c(scoreValues, GINI)
  }
  if("BS" %in% scores){ 
    BS <- mean((pred-as.integer(truth == namePos))^2)
    scoreNames <- c(scoreNames, "Brier score (BS)")
    scoreValues <- c(scoreValues, BS)
  }
  if("PBS" %in% scores){ 
    PBS <- mean((1-pred[truth == namePos])^2)
    scoreNames <- c(scoreNames, "positive Brier score (PBS)")
    scoreValues <- c(scoreValues, PBS)
  }
  if("NBS" %in% scores){ 
    NBS <- mean((1-pred[truth == namePos])^2)
    scoreNames <- c(scoreNames, "negative Brier score (NBS)")
    scoreValues <- c(scoreValues, NBS)
  }
  if("WBS" %in% scores){ 
    stopifnot(length(wBS) == 1)
    if(wBS < 0 | wBS > 1) stop("'wBS' has to be in [0, 1]")
    PBS <- mean((1-pred[truth == namePos])^2)
    NBS <- mean((1-pred[truth == namePos])^2)
    WBS <- wBS*PBS + (1-wBS)*NBS
    scoreNames <- c(scoreNames, "weighted Brier score (WBS)")
    scoreValues <- c(scoreValues, WBS)
  }
  if("BBS" %in% scores){ 
    PBS <- mean((1-pred[truth == namePos])^2)
    NBS <- mean((1-pred[truth == namePos])^2)
    BBS <- 0.5*PBS + 0.5*NBS
    scoreNames <- c(scoreNames, "balanced Brier score (BBS)")
    scoreValues <- c(scoreValues, BBS)
  }

  ## https://scikit-learn.org/stable/modules/model_evaluation.html
  ## https://en.wikipedia.org/wiki/Loss_functions_for_classification
  data.frame(Score = scoreNames, Value = round(scoreValues, digits))
}

GINI <- function(pred, truth, namePos, digits = 3){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    digits = digits, scores = "GINI")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
BS <- function(pred, truth, namePos, digits = 3){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    digits = digits, scores = "BS")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
PBS <- function(pred, truth, namePos, digits = 3){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    digits = digits, scores = "PBS")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
NBS <- function(pred, truth, namePos, digits = 3){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    digits = digits, scores = "NBS")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
WBS <- function(pred, truth, namePos, digits = 3, wBS = 0.5){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    digits = digits, wBS = wBS, scores = "NBS")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
BBS <- function(pred, truth, namePos, digits = 3){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    digits = digits, scores = "BBS")
  res <- tmp[1,2]
  names(res) <- tmp[1,1]
  res
}
