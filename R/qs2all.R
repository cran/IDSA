#' IDSA of all combinations
#'
#' @usage qs2all(y, x, xh, location, method = "fuzzyAND")
#'
#' @param y A numeric vector of a response variable
#' @param x A numeric vector of a explanatory variable
#' @param xh A character variable of an explanatory variable
#' @param location A matrix of spatial locations
#' @param method A character of overlay methods
#'
#' @return A data frame of all possible power of interactive
#' determinants (PID) values from IDSA models.
#'
#' @examples
#' library(GD)
#' x <- sim[,4:6]
#' x.disc <- apply(x, 2, FUN = function(u) disc(u, 4, "quantile"))
#' xh <- do.call(cbind, lapply(1:ncol(x), function(u)
#'   data.frame(cut(x[, u], x.disc[[u]]$itv, include.lowest = TRUE))))
#' names(xh) <- names(x)
#' qidsa.all <- qs2all(y = sim$y, x = x, xh = xh,
#'                     location = sim[, c("lo","la")])
#'
#' @export
#'

qs2all <- function(y, x, xh, location, method = "fuzzyAND"){
  ncx <- ncol(x)
  namex <- names(x)

  # combinations of x
  cox <- list()
  for (i in 1:ncx){
    coxi <- combn(1:ncx, i)
    coxl <- split(coxi, rep(1:ncol(coxi), each = nrow(coxi)))
    cox <- c(cox, coxl)
  }
  names(cox) <- 1:length(cox)

  #

  var <- unlist(lapply(1:length(cox), function(u)
    paste(namex[cox[[u]]], collapse = "_")))
  result <- data.frame("var" = var, "qs2" = rep(NA, length(cox)))

  for (i in 1:length(cox)){
    m <- cox[[i]]
    if (length(m) == 1){
      t <- qs1(y = y, x = x[, m], xh = xh[, m], location = location)
      result[i, 2] <- t[, 2]
    } else {
      fo <- fuzzyoverlay(y = y, layers = xh[, m], method = "fuzzyAND")
      result[i, 2] <- qs2(y = y, x = x[, m], xoverlay = fo$fuzzylayer,
                          location = location)
    }
  }
  class(result) <- "data.frame"
  return(result)
}


