#' Plot Yearly Return
#'
#' yr_plot function will plot yearly return. It require "lubridate" package.
#'
#' @param Return Return Data
#' @return Plot Yearly return
#' @importFrom graphics barplot legend
#' @examples
#' \dontrun{
#'   ret = asset_data
#'   yr_plot(ret)
#'   }
#' @export
yr_plot = function(Return) {
  year_ret = as.data.frame(xts::apply.yearly(Return, PerformanceAnalytics::Return.cumulative))
  rownames(year_ret) = lubridate::year(rownames(year_ret))
  barplot(t(year_ret), col=1:ncol(Return), beside = TRUE, las = 2 )
  if (ncol(Return) > 1)
    legend('topright', colnames(Return), col=1:ncol(Return), lty=NA, lwd = 1, pch = c(15), cex = 0.8)
}
