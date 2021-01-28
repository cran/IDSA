#' Power of interactive determinant for multiple explanatory variables
#' in IDSA model.
#'
#' @usage qs2(y, x, xoverlay, location)
#'
#' @param y A numeric vector of a response variable
#' @param x A numeric vector of a explanatory variable
#' @param xoverlay A character variable of an explanatory variable
#' @param location A matrix of spatial locations
#'
#' @return A power of interactive determinant (PID) value from IDSA model.
#'
#' @examples
#' library(GD)
#' data <- sim[,4:6]
#' data.disc <- apply(data, 2, FUN = function(x) disc(x, 4, "quantile"))
#' layers <- do.call(cbind, lapply(1:ncol(data), function(x)
#'   data.frame(cut(data[, x], data.disc[[x]]$itv, include.lowest = TRUE))))
#' names(layers) <- names(data)
#' fo <- fuzzyoverlay(y = sim[,1], layers = layers, method = "fuzzyAND")
#' q.idsa <- qs2(y = sim$y, x = data, xoverlay = fo$fuzzylayer,
#'               location = sim[, c("lo","la")])
#'
#' @export
#'

qs2 <- function(y, x, xoverlay, location){
  qs_dep <- qs(y, xoverlay, location)
  qs_ind <- qsoverlay(x, xoverlay, location) # debug: remove qsoverlay
  result <- qs_dep/qs_ind
  class(result) <- "numeric"
  return(result)
}




