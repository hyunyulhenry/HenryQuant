#' Calcaulte F-Score of US Firm
#'
#' You should be execute get_US_fs and arrange_US_fs function first
#' @param name the name of arranged Financial statement Data
#' Result of arrange_US_fs function is US_fs
#' @return All listed firm's F-Score
#' @importFrom magrittr "%>%" set_colnames set_rownames
#' @examples
#' \dontrun{
#'  F_US = F_Score_US(US_fs)
#'  }
#' @export
F_Score_US = function(name) {

  if (exists("name") == FALSE) {
    warning("There is no Financial statement data.
            You need to activate get_US_fs and arrange_US_fs first")
  }

  m = nrow(name[[1]])
  ticker_list = rownames(name[[1]])

  ROA = name$'Net Income Before Extra. Items' / name$'Total Assets'
  CFO = name$'Cash from Operating Activities' / name$'Total Assets'
  ACCURUAL = CFO - ROA
  LEV = name$'Total Long Term Debt' / name$'Total Assets'
  LIQ = name$'Total Current Assets' / name$'Total Current Liabilities'
  OFFER = name$'Total Common Shares Outstanding'
  MARGIN = name$'Gross Profit' / name$'Revenue'
  TURN = name$'Revenue' / name$'Total Assets'

  F1 = as.integer(ROA[1] > 0)
  F2 = as.integer(CFO[1] > 0)
  F3 = as.integer(ROA[1] - ROA[2] > 0)
  F4 = as.integer(ACCURUAL[1] > 0)
  F5 = as.integer(LEV[1] - LEV[2] <= 0)
  F6 = as.integer(LIQ[1] - LIQ[2] > 0)
  F7 = as.integer(OFFER[1] - OFFER[2] <= 0)
  F8 = as.integer(MARGIN[1] - MARGIN[2] > 0)
  F9 = as.integer(TURN[1] - TURN[2] > 0)

  item = c("ROA", "CFO", "D ROA", "Accrual", "D Leverage", "D Liquidity",
           "Offer", "D Margin", "D Turnover")

  F_table = cbind(F1, F2, F3, F4, F5, F6, F7, F8, F9) %>%
    set_rownames(ticker_list) %>%
    set_colnames(item)

  F_score = apply(F_table, 1, sum, na.rm = TRUE) %>%
    as.data.frame %>%
    set_rownames(ticker_list) %>%
    set_colnames("F-Score")

  return(F_score)
}
