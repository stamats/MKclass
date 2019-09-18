confMatrix <- function(pred, pred.group, truth, namePos, cutoff = 0.5,
                       relative = TRUE){
  stopifnot(length(pred) == length(truth))
  stopifnot(is.numeric(pred))
  if(!is.factor(truth)) truth <- factor(truth)
  stopifnot(nlevels(truth) == 2)
  if(!is.character(namePos)) namePos <- as.character(namePos)
  stopifnot(namePos %in% levels(truth))
  stopifnot(is.numeric(cutoff))
  stopifnot(length(cutoff) == 1)

  if(missing(pred.group)){
    pred.group <- character(length(pred))
    pred.group[pred >= cutoff] <- namePos
    nam <- levels(truth)
    nameNeg <- nam[nam != namePos]
    pred.group[pred < cutoff] <- nameNeg
    pred.group <- factor(pred.group)
    pred.group <- factor(pred.group, levels = c(nameNeg, namePos))
  }
  stopifnot(length(pred.group) == length(truth))
  if(!is.factor(pred.group)) pred.group <- factor(pred.group)
  stopifnot(nlevels(pred.group) == 2)
  stopifnot(all(levels(truth) %in% levels(pred.group)))
  stopifnot(namePos %in% levels(pred.group))
  
  pred.pos <- pred.group == namePos
  pred.neg <- pred.group != namePos
  truth.pos <- truth == namePos
  truth.neg <- truth != namePos
  TP <- sum(pred.pos & truth.pos)
  TN <- sum(pred.neg & truth.neg)
  FP <- sum(pred.pos & truth.neg)
  FN <- sum(pred.neg & truth.pos)
  
  res.abs <- matrix(c(TP, FN, FP, TN), ncol = 2)
  colnames(res.abs) <- c("Truth positive", "Truth negative")
  rownames(res.abs) <- c("Prediction positive", "Prediction negative")

  res.rel <- t(t(res.abs)/colSums(res.abs))
  
  if(relative){
    res <- list("absolute numbers" = res.abs,
                "relative numbers" = res.rel)
  }else{
    res <- res.abs
  }
  class(res) <- "confMatrix"
  
  return(res)
}
print.confMatrix <- function(x, digits = getOption("digits"), prefix = "\t\t", ...){
  cat("\n")
  cat(strwrap("Confusion Matrix", prefix = prefix), sep = "\n")
  cat(strwrap("----------------", prefix = prefix), sep = "\n")
  cat("\n")
  if(is.matrix(x)) print(x)
  if(is.list(x)){
    cat(strwrap("Absolute numbers", prefix = prefix), sep = "\n")
    cat("\n")
    print(x[[1]], digits = digits, ...)
    cat("\n\n")
    cat(strwrap("Relative numbers", prefix = prefix), sep = "\n")
    cat("\n")
    print(x[[2]], digits = digits, ...)
  }
  invisible(x)
}
