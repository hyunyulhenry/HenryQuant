#' Download all listed firm's ticker in KOREA Markets.
#'
#' This function will Download all listed firm's ticker, name in KOR Markets.
#' @return ticker, name
#' @importFrom utils write.csv
#' @importFrom stringr str_pad
#' @importFrom httr POST
#' @examples
#' \dontrun{
#'  ticker = get_KOR_ticker()
#'  }
#' @export
get_KOR_ticker = function() {

  Sys.setlocale("LC_ALL", "English") # To English

  # kospi = "http://kind.krx.co.kr/corpgeneral/corpList.do?method=download&marketType=stockMkt"
  # kosdaq = "http://kind.krx.co.kr/corpgeneral/corpList.do?method=download&marketType=kosdaqMkt"

  url = 'http://kind.krx.co.kr/corpgeneral/corpList.do'

  kospi = POST(url, encode = 'form', body = list(method = 'download', marketType= 'stockMkt', searchType = '13'))
  kosdaq = POST(url, encode = 'form', body = list(method = 'download', marketType= 'kosdaqMkt', searchType = '13'))

  ks = read_html(kospi) %>% html_table
  kq = read_html(kosdaq) %>% html_table

  Sys.setlocale("LC_ALL", "Korean")

  ks = ks[[1]]
  kq = kq[[1]]

  ks[,2] = str_pad(ks[,2], 6, side="left", pad="0")
  kq[,2] = str_pad(kq[,2], 6, side="left", pad="0")

  ks = cbind(ks[,2], ks[,1], "KOSPI")
  kq = cbind(kq[,2], kq[,1], "KOSDAQ")

  result = rbind(ks, kq)

  write.csv(result, "KOR_ticker_list.csv")
  return(result)

  }
