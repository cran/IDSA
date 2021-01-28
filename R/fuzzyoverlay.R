#' Spatial fuzzy overlay.
#'
#' @usage fuzzyoverlay(y, layers, method = "fuzzyAND")
#'
#' @param y A numeric vector of a response variable
#' @param layers A data frame of spatial layers of explanatory variables.
#' @param method A character of overlay methods,
#' including "fuzzyAND" and "fuzzyOR"
#'
#' @return A data frame of a spatial fuzzy overlay variable.
#'
#' @importFrom stats aggregate
#'
#' @examples
#' library(GD)
#' data <- sim[, 4:6]
#' data.disc <- apply(data, 2, FUN = function(x) disc(x, 4, "quantile"))
#' layers <- do.call(cbind, lapply(1:ncol(data), function(x)
#'   data.frame(cut(data[, x], data.disc[[x]]$itv, include.lowest = TRUE))))
#' names(layers) <- names(data)
#' fo <- fuzzyoverlay(y = sim[,1], layers = layers, method = "fuzzyAND")
#'
#' @export

fuzzyoverlay <- function(y, layers, method = "fuzzyAND"){

  lay.n <- ncol(layers)
  lay.name <- names(layers)

  for (i in 1:lay.n){
    levels.newlayers <- paste(lay.name[i], levels(layers[, i]), sep = "_")
    layers[, i] <- paste(lay.name[i], layers[, i], sep = "_")
    layers[, i] <- factor(layers[, i], levels = levels.newlayers)
  }

  ## fuzzy number is derived from risk mean
  meanrisk.y <- do.call(rbind, lapply(1:lay.n, function(x){
    meanrisk <- aggregate(y, list(layers[, x]), mean)
    meanrisk$layname <- lay.name[x]
    meanrisk
  }))
  meanrisk.y$layname <- factor(meanrisk.y$layname, levels = lay.name)
  normalize <- function(x) (x - min(x))/(max(x) - min(x))
  meanrisk.y[, 2] <- normalize(meanrisk.y[, 2])
  meanrisk.y <- split(meanrisk.y, meanrisk.y$layname)

  fn <- layers # fn is fuzzy number
  for (i in 1:lay.n){
    k <- match(fn[, i], meanrisk.y[[i]][, 1])
    fn[, i] <- meanrisk.y[[i]][k, 2]
  }

  if (method == "fuzzyAND"){
    ## overlay by fuzzy AND: min()
    nobs <- length(y)
    minfn <- do.call(pmin, fn)
    rawlayer <- sapply(1:nobs, function(x) which(fn[x, ] == minfn[x])[1]) # debug: add levels; add [1]
    rawlayer <- factor(lay.name[rawlayer], levels = lay.name)
    levels.layers <- unlist(lapply(1:lay.n, function(x) levels(layers[, x])))
    fuzzylayer <- factor(sapply(1:nobs, function(x) layers[x, rawlayer[x]]), levels = levels.layers)
    fuzzyn <- sapply(1:nobs, function(x) fn[x, rawlayer[x]])
  }
  if (method == "fuzzyOR"){
    ## overlay by fuzzy OR: max()
    nobs <- length(y)
    maxfn <- do.call(pmax, fn)
    rawlayer <- sapply(1:nobs, function(x) which(fn[x, ] == maxfn[x]))
    rawlayer <- factor(lay.name[rawlayer], levels = lay.name)
    levels.layers <- unlist(lapply(1:lay.n, function(x) levels(layers[, x])))
    fuzzylayer <- factor(sapply(1:nobs, function(x) layers[x, rawlayer[x]]), levels = levels.layers)
    fuzzyn <- sapply(1:nobs, function(x) fn[x, rawlayer[x]])
  }

  levels.fuzzylayer <- levels(fuzzylayer)[which(table(fuzzylayer) != 0)]
  fuzzylayer <- factor(fuzzylayer, levels = levels.fuzzylayer)

  z <- data.frame(fuzzylayer = fuzzylayer, fn = fuzzyn,
                  rawlayer = rawlayer)
  class(z) <- "data.frame"
  return(z)
}


