#' Return Low Valuation Firm within specific F-Score Firm in U.S
#'
#' This function return low valuation firm (Low PBR, Low PER, and Hig Dividend Yield)
#' in specific F-Score firm (Such as 8 and 9).
#' You should be execute get_US_fs and arrange_US_fs function first.
#' @param name the name of arranged Financial statement Data
#' @param score F-score you want to screen
#' @param n Number of stocks you want to invest
#' @return Within Entered F-Score Firm, select top N firm based on valuation ratio.
#' @importFrom magrittr "%>%" set_colnames set_rownames
#' @examples
#' \dontrun{
#'  invest = F_Score_value_US(US_fs, score = c(8, 9), n = 30)
#'  }
#' @export
F_Score_value_US = function(name, score = c(8, 9), n = 30) {

  if (exists("name") == FALSE) {
    warning("There is no Financial statement data.
            You need to activate get_US_fs and arrange_US_fs first")
  }

  fscore = F_Score_US(name)
  value = read.csv("value_list.csv", row.names = 1)

  if (sum(rownames(fscore) != rownames(value)) != 0) {
    warnings("FS and Value's rownames are not same")
  }

  value[value<0] = NA

  target = c()
  for (k in 1 : length(score)) {
    temp = which(fscore == score[k])
    target = append(target, temp)
  }
  target = target %>% sort

  rank1 = value[target, 1]  %>% rank
  rank2 = value[target, 2]  %>% rank
  rank3 = -value[target, 3]  %>% rank

  x = target[(cbind(rank1,rank2,rank3) %>% rowSums %>% rank) <= n]

  fscore_bind = cbind(fscore, value)
  res = list(
    TICKER = fscore_bind[x,] %>% rownames,
    DETAIL = round(fscore_bind[x,], 3)
  )

  print(res)
  return(res)

  }
