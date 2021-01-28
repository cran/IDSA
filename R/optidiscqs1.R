#' Strategy 1: Optimal spatial data discretization for individual variables
#' based on SPADE model.
#'
#' @usage optidiscqs1(y, x, location, ndisc, methoddisc)
#'
#' @param y A numeric vector of a response variable
#' @param x A numeric vector of a explanatory variable
#' @param location A matrix of spatial locations
#' @param ndisc A number of discretization
#' @param methoddisc A character of discretization methods
#'
#' @return A list of an optimal spatial discretization using strategy 1.
#'
#' @examples
#' od <- optidiscqs1(y = sim[, 1], x = sim[, 4:6], location = sim[, 2:3],
#'                   ndisc = c(3:5), methoddisc = c("quantile", "equal"))
#'
#' @export

optidiscqs1 <- function(y, x, location, ndisc, methoddisc){
  x <- data.frame(x)
  nx <- ncol(x)
  variable <- colnames(x)

  discindex <- expand.grid(ndisc, methoddisc)
  names(discindex) <- c("n", "method")

  result <- list()
  for (i in 1:nx){
    xx <- x[, i]
    xxh <- do.call(cbind, lapply(1:nrow(discindex), function(u)
      discretize(xx, discindex[u, 1], discindex[u, 2])))
    xx <- replicate(nrow(discindex), xx)
    qs1 <- qs1(y, xx, xxh, location)$qsvalue
    result[[i]] <- cbind(discindex, qs1)
  }
  names(result) <- variable

  # optimal parameters
  bestqs1 <- data.frame(matrix(NA, nx, 3))
  names(bestqs1) <- names(result[[1]])
  for (i in 1:nx){
    k <- which(result[[i]][, 3] == max(result[[i]][, 3]))
    bestqs1[i, ] <- result[[i]][k, ]
  }
  bestqs1 <- cbind(variable, bestqs1)
  bestqs1$method <- methoddisc[bestqs1$method]

  optiresult <- list("qs1" = result, "bestqs1" = bestqs1)
  class(optiresult) <- "list"
  return(optiresult)
}
