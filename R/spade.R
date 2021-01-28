#' SPADE model with spatial discretization parameters.
#'
#' @usage spade(formula, location, data, ndisc, methoddisc)
#'
#' @param formula A formula of spatial variables
#' @param location A character vector of location names in a data frame
#' @param data A data frame of dataset
#' @param ndisc A numeric vector of break numbers for respective
#' explanatory variables
#' @param methoddisc A character vector of discretization methods
#'
#' @return A data frame of power of determinants (PD) of individual
#' variables from SPADE model.
#'
#' @examples
#' q.spade <- spade(formula = y ~ xa + xb + xc, location = c("lo", "la"),
#'                  data = sim, ndisc = c(4,6,6), methoddisc = "quantile")
#'
#' @export

spade <- function(formula, location, data, ndisc, methoddisc){

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
    xh[, i] <- discretize(data[, xnames[i]], ndisc[i], methoddisc[i])
  }

  # qs
  locationdata <- data[, location]
  qs.single <- qs1(response[, 1], explanatory, xh, locationdata)
  class(qs.single) <- "data.frame"
  return(qs.single)
}




