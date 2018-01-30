#' Statistics of Portfolio Return & Risk
#'
#' It calculate statistics related to portfolio return and risk
#' This function mostly use PerformanceAnalytics Package
#'
#' @param R Return Data
#' @importFrom xts as.xts apply.monthly
#' @importFrom zoo rollapply
#' @importFrom PerformanceAnalytics Return.annualized table.AnnualizedReturns UpsideFrequency maxDrawdown Return.cumulative
#' @examples
#' \dontrun{
#'   ret = asset_data
#'   port_stat = Return.stats(ret)
#'   }
#' @export
Return.stats = function(R) {
  R = as.xts(R)
  stat_table = round(
    rbind(Return.annualized(R, geometric = FALSE), table.AnnualizedReturns(R), UpsideFrequency(R), maxDrawdown(R),
          UpsideFrequency(na.omit(rollapply(apply.monthly(R, Return.cumulative), 12, Return.cumulative))),
          UpsideFrequency(na.omit(rollapply(apply.monthly(R, Return.cumulative), 24, Return.cumulative))),
          UpsideFrequency(na.omit(rollapply(apply.monthly(R, Return.cumulative), 36, Return.cumulative)))
    ), 4)
  rownames(stat_table) = c("Ann Ret (Arith)", "Ann Ret (CAGR)", "Ann Std Dev", "Ann Sharpe", "Win Ratio", "MDD",
                           "Roll Win (12M)", "Roll Win (24M)","Roll Win (36M)")
  return(stat_table)
}
