#' IDSA model with spatial discretization parameters.
#'
#' @usage idsa(formula, location, data, ndisc, methoddisc,
#'             methodoverlay = "fuzzyAND")
#'
#' @param formula A formula of spatial variables
#' @param location A character vector of location names in a data frame
#' @param data A data frame of dataset
#' @param ndisc A numeric vector of break numbers for respective
#' explanatory variables
#' @param methoddisc A character vector of discretization methods
#' @param methodoverlay A character of spatial overlay methods, including
#' "fuzzyAND" and "intersection"
#'
#' @return A list of IDSA results.
#'
#' @examples
#' q.fand <- idsa(formula = y ~ xa + xb + xc, location = c("lo", "la"),
#'                data = sim, ndisc = c(4,6,6), methoddisc = "quantile",
#'                methodoverlay = "fuzzyAND")
#' q.ints <- idsa(formula = y ~ xa + xb + xc, location = c("lo", "la"),
#'                data = sim, ndisc = c(4,6,6), methoddisc = "quantile",
#'                methodoverlay = "intersection")
#'
#' @export

idsa <- function(formula, location, data, ndisc, methoddisc,
                 methodoverlay = "fuzzyAND"){

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

  # intersection overlay
  if (methodoverlay == "intersection"){
    overlay <- do.call(paste, c(xh, sep = "_"))
    levels.xh <- lapply(xh[ncolx:1], levels) # debug: [ncolx:1] twice
    levels.xh <- do.call(expand.grid, levels.xh)[ncolx:1]
    newlevels.overlay <- do.call(paste, c(levels.xh, sep = "_"))
    levels.overlay <- unique(overlay)
    levels.overlay2 <- newlevels.overlay[newlevels.overlay %in% levels.overlay]
    overlay <- factor(overlay, levels = levels.overlay2)
  }

  # fuzzy overlay
  if (methodoverlay == "fuzzyAND"){
    newlayers <- fuzzyoverlay(response[, 1], xh, method = methodoverlay)
    overlay <- newlayers$fuzzylayer
  }

  # export qs values
  qs.y <- qs(response[, 1], overlay, locationdata)
  qs.xoverlay <- qsoverlay(explanatory, overlay, locationdata) # debug: remove qsoverlay
  qs.interaction <- qs.y/qs.xoverlay

  # export data
  data$overlay <- overlay
  if (methodoverlay == "fuzzyAND"){
    data$rawx <- newlayers$rawlayer
  }

  result <- list("qs.interaction" = qs.interaction,
                 "qs.y" = qs.y,
                 "qs.xoverlay" = qs.xoverlay,
                 "qs.single" = qs.single,
                 "data" = data)
  class(result) <- "list"
  return(result)
}


















