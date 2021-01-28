#' Strategy 2: Optimal spatial data discretization for individual variables
#' based on SPADE model.
#'
#' @usage loessoptidisc(x, y)
#'
#' @param x A numeric vector of break numbers
#' @param y A numeric vector of q values
#'
#' @return A list of an optimal number of discretization and a plot.
#'
#' @importFrom stats loess predict
#' @importFrom graphics abline axis box lines mtext par
#'
#' @examples
#' lod <- loessoptidisc(x = 4:15, y = log(4:15 + runif(12)))
#'
#' @export

loessoptidisc <- function(x, y){

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  par(new = F)
  par(mar = c(5.1, 4.1, 2.1, 4.1))

  n <- length(y)
  lo <- loess(y ~ x)
  plot(x,y, pch = 16, axes = F,
       xlab = "Break number of discretization", ylab = "75% quantile of Q values")
  box()
  axis(side = 1)
  axis(side = 2, las = 1, col = "red", col.ticks = "red", col.axis = "red")
  lines(x, predict(lo), col = 'red', lwd = 1)
  lo.rate <- c(NA, (lo$fitted[2:n] - lo$fitted[1:(n-1)])/lo$fitted[1:(n-1)])
  par(new = T)
  plot(x, lo.rate, "l", col = 'blue', axes = F, xlab = NA, ylab = NA)
  axis(side = 4, las = 1, col = "blue", col.ticks = "blue", col.axis = "blue")
  mtext("Increase rate", side = 4, line = 3)

  lr1 <- data.frame(n = x, lr = lo.rate, lr.before = c(NA, lo.rate[-n]))
  k <- which(lr1$lr <= 0.05 & lr1$lr.before > 0.05)
  optimal.x <- lr1$n[k]
  optimal.y <- y[k]

  abline(h = 0.05, col = "dodgerblue", lty = 2)
  abline(v = optimal.x, col = "green", lty = 2)

  z <- list("optimal.x" = optimal.x, "optimal.y" = optimal.y)
  class(z) <- "list"

  return(z)
}


