#' PSD with an overlay variable.
#'
#' @usage qsoverlay(x, xoverlay, location)
#'
#' @param x A numeric vector of a explanatory variable
#' @param xoverlay A character variable of an explanatory variable
#' @param location A matrix of spatial locations
#'
#' @return A PSD value of an overlay variable.
#'
#' @examples
#' library(GD)
#' data <- sim[, 4:6]
#' data.disc <- apply(data, 2, FUN = function(x) disc(x, 4, "quantile"))
#' layers <- do.call(cbind, lapply(1:ncol(data), function(x)
#'   data.frame(cut(data[, x], data.disc[[x]]$itv, include.lowest = TRUE))))
#' names(layers) <- names(data)
#' fo <- fuzzyoverlay(y = sim[,1], layers = layers, method = "fuzzyAND")
#' qo <- qsoverlay(x = data, xoverlay = fo$fuzzylayer,
#'                 location = sim[, c("lo","la")])
#'
#' @export
#'

qsoverlay <- function(x, xoverlay, location){

  x <- as.matrix(x)

  nx <- ncol(x)
  sum_nt_h <- c()
  sum_nt_total <- c()
  xoverlay <- as.character(xoverlay)

  count.xoverlay <- table(xoverlay)
  cx1 <- names(which(count.xoverlay == 1))
  k <- which(xoverlay %in% cx1) # debug
  # k <- match(names(count.xoverlay)[which(count.xoverlay == 1)], xoverlay)
  if (length(k) > 0) {
    x <- x[-k, ]
    xo <- xoverlay[-k]
    loc <- location[-k,]
  } else {
    xo <- xoverlay
    loc <- location
  }
  nobs_h <- table(xo)
  name_h <- names(nobs_h)
  n_h <- length(nobs_h)

  for (i in 1:nx){
    xi <- x[, i]

    t_h <- sapply(1:n_h, function(u){
      p <- which(xo == name_h[u])
      tau(xi[p], location[p,])
    })

    t_total <- tau(xi, loc)
    nobs_total <- length(xi)

    sum_nt_h[i] <- sum(nobs_h * t_h) # debug: nobs_h
    sum_nt_total[i] <- nobs_total * t_total
  }

  qsvalue <- 1 - sum(sum_nt_h)/sum(sum_nt_total)
  class(qsvalue) <- "numeric"
  return(qsvalue)
}














