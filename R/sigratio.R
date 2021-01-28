#' Ratio of significantly different zones.
#'
#' @usage sigratio(formula, data, ndisc, methoddisc, methodoverlay = "fuzzyAND")
#'
#' @param formula A formula of spatial variables
#' @param data A data frame of dataset
#' @param ndisc A numeric vector of break numbers for respective
#' explanatory variables
#' @param methoddisc A character vector of discretization methods
#' @param methodoverlay A character of spatial overlay methods, including
#' "fuzzyAND" and "intersection"
#'
#' @return A list of ratios of significantly different zones.
#'
#' @importFrom GD gdrisk
#'
#' @examples
#' sr1 <- sigratio(formula = y ~ xa + xb + xc, data = sim,
#'                ndisc = c(4,4,5), methoddisc = "quantile",
#'                methodoverlay = "fuzzyAND")
#' sr2 <- sigratio(formula = y ~ xa + xb + xc, data = sim,
#'                ndisc = c(4,4,5), methoddisc = "quantile",
#'                methodoverlay = "intersection")
#' sr1$n.zone; sr2$n.zone
#' sr1$ratio.sigdif; sr2$ratio.sigdif
#'
#' @export
#'

sigratio <- function(formula, data, ndisc, methoddisc, methodoverlay = "fuzzyAND"){

  formula <- as.formula(formula)
  formula.vars <- all.vars(formula)
  response <- subset(data, select = formula.vars[1])
  if (formula.vars[2] == "."){
    explanatory <- subset(data, select = -match(formula.vars[1], colnames(data)))
  } else {
    explanatory <- subset(data, select = formula.vars[-1])
  }
  ncolx <- ncol(explanatory)

  xnames <- colnames(explanatory)

  # discretize
  xh <- explanatory
  if (length(ndisc) == 1){
    ndisc <- rep(ndisc, ncolx)
  }
  if (length(methoddisc) == 1){
    methoddisc <- rep(methoddisc, ncolx)
  }
  for (i in 1:ncolx){
    xh[, i] <- discretize(explanatory[, xnames[i]], ndisc[i], methoddisc[i])
  }
  dataxh <- data[, formula.vars]
  dataxh[,-1] <- xh

  if (methodoverlay == "gdinteraction"){
    dataxh$xa_xb <- apply(xh, 1, paste, collapse = "_") #debug
  }
  if (methodoverlay == "intersection"){
    dataxh$xa_xb <- apply(xh, 1, paste, collapse = "_")
  }
  if (methodoverlay == "fuzzyAND"){
    newlayers <- fuzzyoverlay(response[,1], xh, method = "fuzzyAND") #debug
    dataxh$xa_xb <- newlayers$fuzzylayer
  }

  xh.overlayzones <- table(dataxh$xa_xb)
  n.zone <- length(xh.overlayzones)
  k <- which(xh.overlayzones > 1)
  n.zone.xfdz <- length(xh.overlayzones[k])

  # remove n.obs == 1
  k <- which(xh.overlayzones != 1)
  dataxh2 <- dataxh[which(dataxh$xa_xb %in% names(xh.overlayzones)[k]),]

  f2 <- as.formula(paste(formula.vars[1], "xa_xb", sep = "~"))
  gdrisk.zones <- gdrisk(f2, data = dataxh2) ## sig < 0.05
  sigratio.zone <- length(which(gdrisk.zones$xa_xb$sig <= 0.05))/nrow(gdrisk.zones$xa_xb)

  result <- list("n.zone" = n.zone, "n.zone.xFDZ" = n.zone.xfdz,
                 "ratio.sigdif" = sigratio.zone, "gdrisk.zone" = gdrisk.zones,
                 "zonal.n.obs" = xh.overlayzones)
  class(result) <- "list"
  return(result)
}
















