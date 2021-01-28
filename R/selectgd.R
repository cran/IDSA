#' Selecting optimal interaction for GD model.
#'
#' @usage selectgd(formula, data, ndisc, methoddisc)
#'
#' @param formula A formula of spatial variables
#' @param data A data frame of dataset
#' @param ndisc A numeric vector of break numbers for respective
#' explanatory variables
#' @param methoddisc A character vector of discretization methods
#'
#' @return A list of process and results of optimal interaction for
#' GD model.
#'
#' @importFrom GD gd
#' @importFrom stats as.formula
#'
#' @examples
#' s1 <- selectgd(formula = y ~ xa + xb + xc, data = sim,
#'                ndisc = c(4,6,6), methoddisc = "quantile")
#'
#' @export
#'

selectgd <- function(formula, data, ndisc, methoddisc){

  formula <- as.formula(formula)
  formula.vars <- all.vars(formula)
  yvar <- formula.vars[1]
  xvar <- formula.vars[-1]

  response <- subset(data, select = yvar)
  if (formula.vars[2] == "."){
    explanatory <- subset(data, select = -match(yvar, colnames(data)))
  } else {
    explanatory <- subset(data, select = xvar)
  }
  ncolx <- ncol(explanatory)

  xnames <- colnames(explanatory)

  data.gd <- data[, formula.vars]
  # discretize
  xh <- explanatory
  if (length(ndisc) == 1){
    ndisc <- rep(ndisc, ncolx)
  }
  if (length(methoddisc) == 1){
    methoddisc <- rep(methoddisc, ncolx)
  }
  for (i in 1:ncolx){
    xh[, i] <- discretize(data[, xnames[i]], ndisc[i], methoddisc[i])
  }
  data.gd[,-1] <- xh

  g1 <- gd(formula, data.gd)
  g1 <- g1$Factor[, 1:2] # debug for plot
  g1$variable <- as.character(g1$variable)

  result <- list()
  result[[1]] <- g1

  # select best
  m <- 1
  v2 <- xvar
  maxdif <- 1
  selectedvars <- c()
  q.interaction <- c()
  while(maxdif > 0){
    k <- which(g1$qv == max(g1$qv))
    v1 <- g1$variable[k]
    v2 <- v2[-match(v1, v2)]
    selectedvars <- c(selectedvars, v1)

    if (length(v2) == 0){
      break
    }

    g2 <- data.frame(variable = v2, qv = NA)
    g2$variable <- as.character(g2$variable)
    for (u in 1:length(v2)){
      data.gd$xa_xb <- do.call(paste, c(data.gd[, c(selectedvars, v2[u])], sep = "_")) # debug:
      fu <- as.formula(paste(yvar, "xa_xb", sep = "~"))
      gu <- gd(fu, data.gd)
      g2$qv[u] <- gu$Factor$qv
    }
    k <- which(g2$qv == max(g2$qv))
    q.interaction <- g2$qv[k]
    maxdif <- max(g2$qv) - max(g1$qv)
    if (maxdif > 0){
      m <- m + 1
      result[[m]] <- g2
      g1 <- g2
    }
  }

  r3 <- result
  for (i in 1:length(r3)){
    r3[[i]]$addn <- i
  }
  rbinddata <- do.call(rbind, r3)
  rbinddata$variable <- factor(rbinddata$variable, levels = xvar)

  f3 <- as.formula(paste(yvar, paste(selectedvars, collapse = "+"), sep = "~"))

  result2 <- list("selectedvars" = selectedvars, "q.interaction" = q.interaction,
                  "process" = result, "bind.process" = rbinddata,
                  "selected.formula" = f3)
  class(result2) <- "list"
  return(result2)
}
