#' Plot spatial discretization matrix.
#'
#' @usage plotdisc(discmatrix, group)
#'
#' @param discmatrix A matrix of spatial discretization
#' @param group A vector of groups
#'
#' @return A data frame of spatial discretization matrix, which
#' includes mean Q values in each group.
#'
#' @importFrom stats aggregate quantile
#' @importFrom ggplot2 ggplot aes geom_point scale_shape_manual geom_line
#'             theme_bw
#' @importFrom reshape2 melt
#'
#' @examples
#' library(GD)
#' f1 <- formula(NDVIchange ~ Tempchange + Precipitation + Popdensity)
#' odc1 <- optidisc(f1, ndvi_40, discmethod = "quantile", discitv = c(3:20))
#' xvar <- all.vars(f1)[-1]
#' nx <- length(xvar)
#' dm <- do.call(data.frame, lapply(1:nx, function(u) odc1[[u]]$qv.matrix))
#' names(dm) <- xvar
#' pd <- plotdisc(discmatrix = dm, group = rep(1:6, each = 3))
#'
#' @export
#'

plotdisc <- function(discmatrix, group){
  value <- variable <- NULL
  dm <- data.frame(discmatrix)
  nx <- ncol(dm)
  dm.group <- aggregate(x = dm, by = list(group), FUN = "mean")
  names(dm.group)[1] <- "group"

  dm.melt <- melt(dm.group, id = "group")
  p1 <- ggplot(dm.melt, aes(x = group, y = value,
                      color = variable, shape = variable)) +
    geom_point() +
    scale_shape_manual(values = 1:nx) +
    geom_line() +
    theme_bw()
  print(p1)
  if (nx == 1){
    dm.group$quantile75 <- quantile(dm.group[,2], 0.75)
  } else {
    dm.group$quantile75 <- apply(dm.group[,-1], 1, quantile, 0.75)
  }
  class(dm.group) <- "data.frame"
  return(dm.group)
}



















