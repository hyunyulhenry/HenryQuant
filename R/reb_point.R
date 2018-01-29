#' Return Rebalancing Point by Period
#'
#' This function will return rebalacing period, from START to END by REBALANCING PERIOD.
#' START and END need specific date and Period shold be number.
#'
#' @param R Return Data
#' @param start Starting Date
#' @param end Ending Date
#' @param period Rebalancing Period by Number
#' @importFrom xts as.xts endpoints
#' @importFrom zoo index
#' @examples
#'   ret = asset_data
#'   reb_time = reb_point(ret, "1994-12-31", "2016-12-31", 3)
#' @export
reb_point = function(R, start, end, period) {
  R = as.xts(R)
  ep = endpoints(R, on = "months")
  point = seq(which(index(R)[ep] == start), which(index(R)[ep] == end), by = period)
  return(point)
}
