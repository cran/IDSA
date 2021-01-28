#' Spatial discretization.
#'
#' @usage discretize(x, n, method)
#'
#' @param x A numeric vector to be discretized
#' @param n A number of breaks
#' @param method A character of discretization method
#'
#' @return A vector of discretized variable of \code{x}.
#'
#' @importFrom GD disc
#'
#' @examples
#' x.disc <- discretize(x = runif(12), n = 3, method = "quantile")
#' table(x.disc)
#'
#' @export

discretize <- function(x, n, method){
  odcx <- disc(x, n, method)
  z <- cut(x, odcx$itv, include.lowest = TRUE)
  class(z) <- "factor"
  return(z)
}

