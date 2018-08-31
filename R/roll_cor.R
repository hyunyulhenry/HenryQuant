#' Calculate & Plot Rolling Correlation for two assets.
#'
#' This function will caculate and plot rolling correlation between
#' return of two assets.
#'
#' This Package is based on PerformanceAnalytics
#'
#' @param Ra an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param Rb an xts, vector, matrix, data frame, timeSeries or zoo object of asset returns
#' @param width number of periods to apply rolling function window over
#' @importFrom zoo rollapply
#' @importFrom stats cor
#' @examples
#'   ret = asset_data[,c(1,5)]
#'   roll_cor(ret[,1], ret[,2], width = 360)
#' @export
roll_cor = function(Ra, Rb, width) {

  result = rollapply(cbind(Ra, Rb),
                       width,
                       function(x) {cor(x[,1], x[,2])},
                       by.column = FALSE
                       )
  return(result)

}
