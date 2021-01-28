#' Power of spatial determinant (PSD).
#'
#' @usage qs(y, xh, location)
#'
#' @param y A numeric vector of a response variable
#' @param xh A character variable, a data frame or a matrix of
#' explanatory variables
#' @param location A matrix of spatial locations
#'
#' @return A power of spatial determinant (PSD) value.
#'
#' @examples
#' # an explanatory variable
#' library(GD)
#' data.disc <- disc(sim$xa, 4, "quantile")
#' xh <- cut(sim$xa, data.disc$itv, include.lowest = TRUE)
#' qs(sim$y, xh, location = sim[, c("lo","la")])
#' # multiple explanatory variables
#' data <- sim[,4:6]
#' data.disc <- apply(data, 2, FUN = function(x) disc(x, 4, "quantile"))
#' xh <- do.call(cbind, lapply(1:ncol(data), function(x)
#'   data.frame(cut(data[, x], data.disc[[x]]$itv, include.lowest = TRUE))))
#' names(xh) <- names(data)
#' qs(sim$y, xh, location = sim[, c("lo","la")])
#'
#' @export

qs <- function(y, xh, location){

  xh <- as.matrix(xh)

  ncol.xh <- ncol(xh)
  nt <- c()
  for (v in 1:ncol.xh){
    xhv <- as.character(xh[, v])
    count.xhv <- table(xhv)

    k <- match(names(count.xhv)[which(count.xhv == 1)], xhv)
    if (length(k) > 0) {
      y <- y[-k]
      xhv <- xhv[-k]
      location <- location[-k,]
    }

    nobs_h <- table(xhv)
    name_h <- names(nobs_h)
    n_h <- length(nobs_h)

    t_h <- sapply(1:n_h, function(u){
      k <- which(xhv == name_h[u])
      tau(y[k], location[k,])
    })
    nt[v] <- sum(nobs_h * t_h)
  }

  t_total <- tau(y, location)
  nobs_total <- length(y)

  qsvalue <- 1 - nt/(nobs_total * t_total)
  class(qsvalue) <- "numeric"
  return(qsvalue)
}







