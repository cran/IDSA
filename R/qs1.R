#' Power of spatial and multilevel discretization determinant (PSMD)
#' of SPADE model for an individual explanatory variable.
#'
#' @usage qs1(y, x, xh, location)
#'
#' @param y A numeric vector of a response variable
#' @param x A numeric vector of a explanatory variable
#' @param xh A character variable of an explanatory variable
#' @param location A matrix of spatial locations
#'
#' @return A data frame of PSMD values.
#'
#' @examples
#' library(GD)
#' data.disc <- disc(sim$xa, 4, "quantile")
#' xh <- cut(sim$xa, data.disc$itv, include.lowest = TRUE)
#' qs1(y = sim$y, x = sim$xa, xh = xh, location = sim[, c("lo","la")])
#'
#' @export
#'

qs1 <- function(y, x, xh, location){
  x <- data.frame(x); xh <- data.frame(xh)
  n <- ncol(x)
  namesx <- names(x)

  qs_dep <- qs(y, xh, location)
  # qs_ind <- mclapply(1:n, function(u){qs(x[, u], xh[, u], location)},
                       # mc.cores = 2) # debug: use parallel
  # qs_ind <- unlist(qs_ind)
  qs_ind <- sapply(1:n, function(u) qs(x[, u], xh[, u], location))
  qsvalue <- qs_dep/qs_ind

  result <- data.frame(variable = namesx, qsvalue)
  class(result) <- "data.frame"
  return(result)
}





