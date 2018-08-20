#' Calculate Strenghness of EPS Momentum
#'
#' This function calculates how stable the EPS has grown.
#' @param EPS Earnings Per Share
#'
#' @examples
#' \dontrun{
#' EPS = c(3506,2713,2198,2735,5421)
#' eps.mom.strength(EPS)
#'  }
#' @export
eps.mom.strength = function(EPS) {

  EPS = as.numeric(EPS)

  d1 = tryCatch({
    reg = lm(EPS ~ c(1:length(EPS)))
    d1 = reg$coefficients[2]
  }, error = function(e) {NA} )

  d2 = mean(abs(EPS))

  result = as.numeric(d1 / d2)
  return(result)

}
