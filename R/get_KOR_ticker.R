#' Download all listed firm's ticker in KOREA Markets.
#'
#' This function will Download all listed firm's ticker, name in KOR Markets.
#' @return ticker, name
#' @importFrom utils download.file read.csv
#' @examples
#' \dontrun{
#'  get_KOR_ticker()
#'  }
#' @export
get_KOR_ticker = function() {

  Sys.setlocale("LC_ALL", "Korean")

  url = "http://www.sejongdata.com/query/value.html"

  temp = xml2::read_html(url,encoding = "UTF-8")
  data = temp %>% html_nodes(".bus_board_txt1") %>% html_text
  item = temp %>% html_nodes('.bus_board_tit1') %>% html_text

  ticker = data.frame(matrix(data ,ncol=5, byrow=T))
  names(ticker) = item
  ticker = ticker[,c(1,2)]
  rm(data, item, temp, url)

  write.csv(ticker, "KOR_ticker_list.csv")
  return(ticker)

}
