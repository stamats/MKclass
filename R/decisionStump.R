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
  model <- function(x){
    factor(ifelse(x > optcut[1], namePos, nameNeg))
  }
  res <- list(call = cl, cutoff = optcut[1], perfMeasure = optcut[2],
              model = model, fitted = model(pred), 
              data = data.frame(pred, truth))
  class(res) <- "decisionStump"
  res
}