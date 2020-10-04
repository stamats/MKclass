perfScores <- function(pred, truth, namePos, wBS = 0.5, scores = "all",
                       transform = FALSE){
  stopifnot(length(pred) == length(truth))
  stopifnot(is.numeric(pred))

  if(!is.factor(truth)) truth <- factor(truth)
  stopifnot(nlevels(truth) == 2)
  
  if(!is.character(namePos)) namePos <- as.character(namePos)
  stopifnot(namePos %in% levels(truth))
  
  if(any(is.na(pred)) || any(is.na(truth))){
    warning("NA values were removed!")
    no.NA <- !is.na(pred) & !is.na(truth)
    pred <- pred[no.NA]
    truth <- truth[no.NA]
  }
  if(transform){
    fit <- glm(truth ~ pred, family = binomial)
    pred <- predict(fit, type = "response")
  }
  
  scoreNames <- NULL
  scoreValues <- NULL
  if(any(scores == "all")){
    scores <- c("AUC", "GINI", "BS", "PBS", "NBS", "WBS", "BBS")
  }

  if("AUC" %in% scores){ 
    AUC <- AUC(pred, group = as.integer(truth == namePos))
    names(AUC) <- NULL
    scoreNames <- c(scoreNames, "area under curve (AUC)")
    scoreValues <- c(scoreValues, AUC)
  }
  if("GINI" %in% scores){ 
    AUC <- AUC(pred, group = as.integer(truth == namePos))
    names(AUC) <- NULL
    GINI <- 2*AUC - 1
    scoreNames <- c(scoreNames, "Gini index (GINI)")
    scoreValues <- c(scoreValues, GINI)
  }
  if("BS" %in% scores){ 
    if(any(pred > 1) | any(pred < 0)){
      warning("There are predictions outside the interval [0,1]. BS is not valid!")
    }
    BS <- mean((pred-as.integer(truth == namePos))^2)
    scoreNames <- c(scoreNames, "Brier score (BS)")
    scoreValues <- c(scoreValues, BS)
  }
  if("PBS" %in% scores){ 
    if(any(pred > 1) | any(pred < 0)){
      warning("There are predictions outside the interval [0,1]. PBS is not valid!")
    }
    PBS <- mean((1-pred[truth == namePos])^2)
    scoreNames <- c(scoreNames, "positive Brier score (PBS)")
    scoreValues <- c(scoreValues, PBS)
  }
  if("NBS" %in% scores){ 
    if(any(pred > 1) | any(pred < 0)){
      warning("There are predictions outside the interval [0,1]. NBS is not valid!")
    }
    NBS <- mean((pred[truth != namePos])^2)
    scoreNames <- c(scoreNames, "negative Brier score (NBS)")
    scoreValues <- c(scoreValues, NBS)
  }
  if("WBS" %in% scores){ 
    if(any(pred > 1) | any(pred < 0)){
      warning("There are predictions outside the interval [0,1]. WBS is not valid!")
    }
    stopifnot(length(wBS) == 1)
    if(wBS < 0 | wBS > 1) stop("'wBS' has to be in [0, 1]")
    PBS <- mean((1-pred[truth == namePos])^2)
    NBS <- mean((pred[truth != namePos])^2)
    WBS <- wBS*PBS + (1-wBS)*NBS
    scoreNames <- c(scoreNames, "weighted Brier score (WBS)")
    scoreValues <- c(scoreValues, WBS)
  }
  if("BBS" %in% scores){ 
    if(any(pred > 1) | any(pred < 0)){
      warning("There are predictions outside the interval [0,1]. BBS is not valid!")
    }
    PBS <- mean((1-pred[truth == namePos])^2)
    NBS <- mean((pred[truth != namePos])^2)
    BBS <- 0.5*PBS + 0.5*NBS
    scoreNames <- c(scoreNames, "balanced Brier score (BBS)")
    scoreValues <- c(scoreValues, BBS)
  }

  ## https://scikit-learn.org/stable/modules/model_evaluation.html
  ## https://en.wikipedia.org/wiki/Loss_functions_for_classification
  res <- list(score = scoreNames, value = scoreValues)
  class(res) <- "perfScore"
  res
}
print.perfScore <- function(x, digits = getOption("digits"), prefix = "\t\t", ...){
  cat("\n")
  cat(strwrap("Performance Score(s)", prefix = prefix), sep = "\n")
  cat("\n")
  print(data.frame(Score = x$score, Value = x$value))
  invisible(x)
}

GINI <- function(pred, truth, namePos){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    scores = "GINI")
  res <- tmp$value
  names(res) <- tmp$score
  res
}
BS <- function(pred, truth, namePos){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    scores = "BS")
  res <- tmp$value
  names(res) <- tmp$score
  res
}
PBS <- function(pred, truth, namePos){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    scores = "PBS")
  res <- tmp$value
  names(res) <- tmp$score
  res
}
NBS <- function(pred, truth, namePos){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    scores = "NBS")
  res <- tmp$value
  names(res) <- tmp$score
  res
}
WBS <- function(pred, truth, namePos, wBS = 0.5){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    wBS = wBS, scores = "NBS")
  res <- tmp$value
  names(res) <- tmp$score
  res
}
BBS <- function(pred, truth, namePos){
  tmp <- perfScores(pred = pred, truth = truth, namePos = namePos, 
                    scores = "BBS")
  res <- tmp$value
  names(res) <- tmp$score
  res
}
