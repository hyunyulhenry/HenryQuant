#' Caculated Zephyr K-Ratio
#'
#' It calculate Zephyr K-Ratio which is consistency of an equity's return over time
#' K-Ratio is slope of trend line divided by standard error of trend line
#' More information can be found at:
#' http://s3.amazonaws.com/zanran_storage/www.styleadvisor.com/ContentPages/2449998087.pdf
#'
#' @param R Return Data
#' @importFrom xts as.xts endpoints
#' @importFrom graphics plot
#' @importFrom stats coef lm na.action na.exclude na.omit
#'
#' @examples
#' \dontrun{
#'   ret = asset_data
#'   K.ratio(ret[,1])
#'   }
#' @export
K.ratio = function(R) {

  R = as.xts(R)
  R = na.omit(R)

  x = rep(1:length(R))
  y = Return.CumSeries(R)

  reg = lm(y ~ x, na.action(na.exclude))
  ratio = coef(summary(reg))[2, 1] / coef(summary(reg))[2, 2]

  reg_line = xts(as.numeric(reg$fitted.values), order.by = index(reg$fitted.values))
  reg.plot = plot(cbind(y, reg_line), lwd=3, las=1, main = "")

  return(list(ratio, reg.plot))

}
