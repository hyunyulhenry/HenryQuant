#' Market Cap Weighted & Equal Weighted Portfolio
#'
#' This function solves for market cap weights portfolio & equal weights
#' The upper limit for market cap portfolio can be set.
#' @param MarketCap Market Capital
#' @param MaxWeight Maximum weight for market cap weighted portfolio.
#' If not entered, twice the equal weight is applied
#' @param type "VW" for value(cap) weight, "EW" for equal weight
#' @examples
#' \dontrun{
#'   wt_normal(MarketCap, MaxWeight=0.05, type = "VW")
#'   }
#' @export
wt_normal = function(MarketCap, MaxWeight = NULL, type = "VW") {

  MarketCap = as.numeric(MarketCap)
  MarketCap = MarketCap / sum(MarketCap)

  if (is.null(MaxWeight)) {
    MaxWeight = 1/length(MarketCap) * 2
  }

  if (type == "VW") {
    wt = MarketCap / sum(MarketCap)
    while (max(wt) > MaxWeight) {
      wt[wt > MaxWeight] = MaxWeight
      wt = wt / sum(wt)
    }
  }

  if (type == "EW") {
    len = length(MarketCap)
    wt = rep((1/len),len)
  }

  return(wt)
}

