

train <- function(mdlName, data, dvName) {
  mdl <- NULL
  if(mdlName == "rf") {
    mdl <- .trainRF(data, dvName)
  } else if(mdlName == "lm") {
    mdl <- .trainLM(data, dvName)
  } else {
    stop("Model not implemented!")
  }
  return(mdl)
}

.trainRF <- function(data, dvName) {
  fml <- as.formula(paste0(dvName, " ~ ."))
  mdl <- ranger(fml, data)
  return(mdl)
}

.trainLM <- function(data, dvName) {
  fml <- as.formula(paste0(dvName, " ~ ."))
  mdl <- lm(fml, data)
  return(mdl)
}