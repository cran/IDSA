#' Spatial dependence parameter.
#'
#' @usage tau(y, location)
#'
#' @param y A numeric vector of a response variable
#' @param location A matrix of spatial locations
#'
#' @return A value of spatial dependence parameter.
#'
#' @importFrom stats dist
#' @importFrom utils combn
#'
#' @examples
#' tau(y = sim[, 1], location = sim[, 2:3])
#'
#' @export

tau <- function(y, location){
  location <- as.matrix(location)

  ny <- length(y)
  id <- 1:ny

  # dist
  distij <- dist(location)
  wij <- 1/distij
  # cij
  comb2.int <- function(n, rep = FALSE){
    if(!rep){
      # e.g. n=3 => (1,2), (1,3), (2,3)
      x <- rep(1:n,(n:1)-1)
      i <- seq_along(x)+1
      o <- c(0,cumsum((n-2):1))
      y <- i-o[x]
    }else{
      # e.g. n=3 => (1,1), (1,2), (1,3), (2,2), (2,3), (3,3)
      x <- rep(1:n,n:1)
      i <- seq_along(x)
      o <- c(0,cumsum(n:2))
      y <- i-o[x]+x-1
    }
    return(cbind(x,y))
  }
  nyc <- comb2.int(ny)
  y12 <- y[nyc[, 1]] - y[nyc[, 2]]

  cij <- y12 * y12 /2

  # tau
  z <- sum(wij * cij)/sum(wij)
  class(z) <- "numeric"
  return(z)
}



