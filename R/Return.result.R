#' Calculate returns & turnover for a portfolio of assets
#'
#' This function using Return.Portfolio function
#'
#' Using a time series of returns and weights for each asset,
#' this function calculates the returns of a portfolio with the
#' Gross Return, Net Return (after fee), and Turnover of Portfolio.
#'
#' @param R An xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param weight A time series or single-row matrix/vector containing asset weights, as decimal percentages, treated as beginning of period weights.
#' @param fee Buy/Sell fee, cost, and tax.
#' @return gross is before cost, net is after cost, and turnover is turnover rate
#' @importFrom stats lag
#' @importFrom PerformanceAnalytics Return.portfolio
#' @importFrom xts xts
#' @importFrom zoo index
#' @examples
#'   ret = asset_data[,c(1,5)]
#'   portfolio = Return.result(ret)
#'   port_gross = portfolio$gross
#'   port_net = portfolio$net
#'   port_turnover = portfolio$turnover
#' @export
Return.result = function(R, weight=NULL, fee = 0) {

  R_verbose = Return.portfolio(R, weight, verbose = TRUE)

  gross = R_verbose$returns
  turnover = xts(rowSums(abs(R_verbose$BOP.Weight - lag(R_verbose$EOP.Weight)), na.rm = TRUE), order.by = index(R_verbose$BOP.Weight))
  net = R_verbose$returns - (turnover * fee)

  result = list(
    gross = gross,
    turnover = turnover,
    net = net
  )

  return(result)
}
