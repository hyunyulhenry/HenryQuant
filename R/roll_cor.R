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
#' @importFrom PerformanceAnalytics checkData chart.RollingCorrelation
#' @importFrom zoo rollapply
#' @importFrom stats na.omit cor time
#' @examples
#' \dontrun{
#'   ret = asset_data[,c(1,5)]
#'   roll_cor(ret[,1], ret[,2], width = 360)
#' }
#' @export

roll_cor = function(Ra, Rb, width) {

  Ra = checkData(Ra)
  Rb = checkData(Rb)
  columns.a = ncol(Ra)
  columns.b = ncol(Rb)
  columnnames.a = colnames(Ra)
  columnnames.b = colnames(Rb)
  for (column.a in 1:columns.a) {
    for (column.b in 1:columns.b) {
      merged.assets = merge(Ra[, column.a, drop = FALSE],
                            Rb[, column.b, drop = FALSE])
      column.calc = rollapply(na.omit(merged.assets[, ,
                                                    drop = FALSE]), width = width, FUN = function(x) cor(x[,
                                                                                                           1, drop = FALSE], x[, 2, drop = FALSE]), by = 1,
                              by.column = FALSE, align = "right")
      column.calc.tmp = xts(column.calc)
      colnames(column.calc.tmp) = paste(columnnames.a[column.a],
                                        columnnames.b[column.b], sep = " to ")
      column.calc = xts(column.calc.tmp, order.by = time(column.calc))
      if (column.a == 1 & column.b == 1)
        Result.calc = column.calc
      else Result.calc = merge(Result.calc, column.calc)
    }
  }
  chart.RollingCorrelation(Ra, Rb, width)
  return(Result.calc)
}
