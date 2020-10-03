optCutoff <- function(pred, truth, namePos,
                      perfMeasure = "YJS",
                      MAX = TRUE, parallel = FALSE, ncores,
                      delta = 0.01, ...){
  stopifnot(length(pred) == length(truth))
  stopifnot(is.numeric(pred))
  if(!is.factor(truth)) truth <- factor(truth)
  stopifnot(nlevels(truth) == 2)
  if(!is.character(namePos)) namePos <- as.character(namePos)
  stopifnot(namePos %in% levels(truth))
  stopifnot(length(perfMeasure) == 1)

  W <- range(pred)
  cutoffs <- c(W[1]-delta, pred, W[2]+delta)
  if(parallel){
    stopifnot(requireNamespace("foreach", quietly = TRUE))
    stopifnot(requireNamespace("parallel", quietly = TRUE))
    stopifnot(requireNamespace("doParallel", quietly = TRUE))
    if(missing(ncores)){
      cores <- parallel::detectCores()
      cl <- parallel::makeCluster(cores[1]-1)
    }else{
      cl <- parallel::makeCluster(ncores)
    }
    doParallel::registerDoParallel(cl)
      `%dopar%` <- foreach::`%dopar%`
      perfs <- foreach::foreach(i = seq_along(cutoffs)) %dopar% {
      MKclass::perfMeasures(pred = pred, truth = truth, namePos = namePos,
                            cutoff = cutoffs[i], measures = perfMeasure, ...)$value
    }
    parallel::stopCluster(cl)
  }else{
    perfs <- numeric(length(cutoffs))
    for(i in seq_along(cutoffs)){
      perfs[i] <- perfMeasures(pred = pred, truth = truth,
                               namePos = namePos,
                               cutoff = cutoffs[i], 
                               measures = perfMeasure, ...)$value
    }
  }
  if(MAX){
    ind.max <- which.max(perfs)
    res <- unlist(c(cutoffs[ind.max], perfs[ind.max]))
  }else{
    ind.min <- which.min(perfs)
    res <- c(cutoffs[ind.min], perfs[ind.min])
  }
  names(res) <- c("Optimal Cut-off", perfMeasure)
  res
}
