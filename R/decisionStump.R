decisionStump <- function(pred, truth, namePos, perfMeasure = "YJS", 
                          MAX = TRUE, parallel = FALSE, ncores,
                          delta = 0.01, ...){
  cl <- match.call()
  optcut <- optCutoff(pred = pred, truth = truth, namePos = namePos,
                      perfMeasure = perfMeasure, MAX = MAX, 
                      parallel = parallel, ncores = ncores, 
                      delta = delta, ...)
  if(!is.factor(truth)) truth <- factor(truth)
  nameNeg <- levels(truth)[levels(truth) != namePos]
  model <- function(x){}
  OC <- optcut[1]
  names(OC) <- NULL
  body(model) <- substitute({factor(ifelse(x > OC, namePos, nameNeg))}, 
                            list(OC = OC,
                                 namePos = as.character(namePos),
                                 nameNeg = nameNeg))
  res <- list(call = cl, cutoff = optcut[1], perfMeasure = optcut[2],
              model = model, fitted = model(pred), 
              data = data.frame(pred, truth))
  class(res) <- "decisionStump"
  res
}
print.decisionStump <- function(x, digits = max(3L, getOption("digits") - 3L), ...){
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Cut-off:\n")
  print.default(format(x$cutoff, digits = digits), print.gap = 2L, 
                quote = FALSE)
  cat("\nPerformance:\n")
  print.default(format(x$perfMeasure, digits = digits), print.gap = 2L, 
                quote = FALSE)
  invisible(x)
}
predict.decisionStump <- function(object, newdata, ...){
  if(missing(newdata) || is.null(newdata))
    predictor <- object$fitted
  else
    predictor <- object$model(newdata)
  predictor
}