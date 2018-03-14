#' Calculate Cumulative Return Series
#'
#' @param R Return Data
#' @importFrom xts as.xts
#' @examples
#'   ret = asset_data
#'   cum_series = Return.CumSeries(ret)
#' @export
Return.CumSeries = function(R) {
  R = as.xts(R)
  R_cum = cumprod(1+R)-1

  return(R_cum)
}
