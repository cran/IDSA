#' Selecting optimal interaction for IDSA model.
#'
#' @usage selectidsa(formula, data, location, ndisc, methoddisc)
#'
#' @param formula A formula of spatial variables
#' @param data A data frame of dataset
#' @param location A character vector of location names in a data frame
#' @param ndisc A numeric vector of break numbers for respective
#' explanatory variables
#' @param methoddisc A character vector of discretization methods
#'
#' @return A list of process and results of optimal interaction for
#' IDSA model.
#'
#' @importFrom stats as.formula
#'
#' @examples
#' sim$xd <- log(sim$xa * sim$xb)
#' s1 <- selectidsa(formula = y ~ xa + xb + xc + xd, data = sim,
#'                  location = c("lo", "la"),
#'                  ndisc = c(4,6,6,5), methoddisc = "quantile")
#'
#' @export
#'

selectidsa <- function(formula, data, location, ndisc, methoddisc){
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

  location.data <- data[, location]

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

  # spade for single variable
  h1 <- qs1(response[, 1], explanatory, xh, location.data) # debug
  h1$variable <- as.character(h1$variable)

  select.matrix <- h1
  select.matrix$var1 <- "individual"

  # select best
  v2 <- xvar

  q.result <- data.frame(matrix(NA, ncolx, 3))
  names(q.result) <- c("add.var", "interaction", "q.idsa")

  k <- which(h1$qsvalue == max(h1$qsvalue))
  v1 <- h1$variable[k]

  q.result$add.var[1] <- h1$variable[k]
  q.result$interaction[1] <- h1$variable[k]
  q.result$q.idsa[1] <- h1$qsvalue[k]

  ovar <- v2[-match(v1, v2)]
  gvar <- v1

  for (i in 1:(ncolx - 1)){
    selection <- selectaddavar(response[, 1], explanatory, xh,
                               location.data,
                               x.given = gvar, x.option = ovar,
                               method = "fuzzyAND")
    q.result$add.var[i + 1] <- selection$max.var
    q.result$interaction[i + 1] <- paste(c(gvar, selection$max.var),
                                  collapse = "_")
    q.result$q.idsa[i + 1] <- selection$max.qs.interaction

    smi <- selection$qs.interaction
    smi$var1 <- gvar[length(gvar)]

    select.matrix <- rbind(select.matrix, smi)

    gvar <- c(gvar, selection$max.var)
    ovar <- ovar[-match(selection$max.var, ovar)]
  }

  k <- which(q.result$q.idsa == max(q.result$q.idsa))
  selectedvars <- q.result$add.var[1:k]
  q.interaction <- q.result$q.idsa[k]

  f3 <- as.formula(paste(yvar, paste(selectedvars, collapse = "+"), sep = "~"))

  select.matrix <- select.matrix[, c(3,1,2)]
  names(select.matrix)[2] <- "var2"
  select.matrix$var1 <- factor(select.matrix$var1,
                               levels = unique(select.matrix$var1))
  select.matrix$var2 <- factor(select.matrix$var2, levels = gvar)

  result2 <- list("selected.formula" = f3,
                  "selectedvars" = selectedvars, "q.interaction" = q.interaction,
                  "process" = q.result, "process.matrix" = select.matrix)
  class(result2) <- "list"
  return(result2)
}

