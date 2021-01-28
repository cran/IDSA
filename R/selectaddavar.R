#' Selecting and adding a variable to improve PID.
#'
#' @usage selectaddavar(y, x, xh, location, x.given, x.option,
#'                      method = "fuzzyAND")
#'
#' @param y A numeric vector of a response variable
#' @param x A data frame or a matrix of explanatory variables
#' @param xh A data frame or a matrix of discretized explanatory variables
#' @param location A data frame of locations
#' @param x.given A name of a start variable
#' @param x.option A character vector of names of optional variables
#' @param method A character of spatial overlay method
#'
#' @return A list of process data of improving PID values by adding a variable.
#'
#' @examples
#' library(GD)
#' x <- sim[, 4:6]
#' x.disc <- apply(x, 2, FUN = function(u) disc(u, 4, "quantile"))
#' xh <- do.call(cbind, lapply(1:ncol(x), function(u)
#'   data.frame(cut(x[, u], x.disc[[u]]$itv, include.lowest = TRUE))))
#' names(xh) <- names(x)
#' sav <- selectaddavar(y = sim[, 1], x = x, xh = xh,
#'                      location = sim[, c("lo","la")],
#'                      x.given = "xc", x.option = c("xa", "xb"),
#'                      method = "fuzzyAND")
#' @export
#'

selectaddavar <- function(y, x, xh, location, x.given, x.option,
                          method = "fuzzyAND"){
  nxg <- length(x.given)
  nxo <- length(x.option)

  if (nxg == 1){
    q0 <- qs1(y, x[, x.given], xh[, x.given], location)
    q0 <- q0$qsvalue
  } else if (method == "intersection"){
    # q0
    overlay <- do.call(paste, c(xh[, x.given], sep = "_"))
    qs.y <- qs(y, overlay, location)
    qs.xoverlay <- qsoverlay(x[, x.given], overlay, location)
    q0 <- qs.y/qs.xoverlay
  } else {
    # q0
    newlayers <- fuzzyoverlay(y, xh[, x.given], method = method)
    overlay <- newlayers$fuzzylayer
    qs.y <- qs(y, overlay, location)
    qs.xoverlay <- qsoverlay(x[, x.given], overlay, location)
    q0 <- qs.y/qs.xoverlay
  }

  # added
  h2 <- data.frame(variable = x.option, qsvalue = NA)
  h2$variable <- as.character(h2$variable)
  for (u in 1:nxo){
    if (method == "intersection"){
      overlay <- do.call(paste, c(xh[, c(x.given, x.option[u])], sep = "_"))
    }
    if (method == "fuzzyAND"){
      newlayers <- fuzzyoverlay(y, xh[, c(x.given, x.option[u])], method = "fuzzyAND")
      overlay <- newlayers$fuzzylayer
    }
    qs.y <- qs(y, overlay, location) # debug: new qs.y for y on new overlay
    qs.xoverlay <- qsoverlay(x[, c(x.given, x.option[u])], overlay, location)
    h2$qsvalue[u] <- qs.y/qs.xoverlay
    qs2(y = y, x = x[, c(x.given, x.option[u])], xoverlay = overlay,
        location = location)
  }
  k <- which(h2$qsvalue == max(h2$qsvalue))
  maxvar <- x.option[k]
  maxqs <- h2$qsvalue[k]
  if (maxqs > q0) {
    addvar <- maxvar
    addqs <- maxqs
  } else {
    addvar <- c()
    addqs <- c()
  }

  z <- list("best.var" = addvar, "best.qs.interaction" = addqs,
            "qs.interaction" = h2,
            "max.var" = maxvar, "max.qs.interaction" = maxqs)
  class(z) <- "list"
  return(z)
}









