#' Download individual firm's price in Korea Markets.
#'
#' This function will Download all listed firm's Adjusted price in Korea Markets.
#'
#' It will aumomatically save individual stock prices
#'
#' @param ticker symbol for stock
#' @param days periods of days (default to 3000)
#'
#' @importFrom utils write.csv
#' @importFrom xts xts as.xts
#' @importFrom zoo na.locf
#' @importFrom lubridate ymd today
#' @importFrom magrittr "%>%"
#' @importFrom httr GET
#' @importFrom xml2 read_html
#' @importFrom rvest html_node
#' @importFrom readr read_delim
#' @importFrom timetk tk_xts
#' @examples
#' \dontrun{
#'  get_price(ticker = '005930', days = 3000)
#'  }
#' @export
get_price = function(ticker = "005930", days = 3000) {

  price = xts(NA, order.by = Sys.Date())

  url = paste0("https://fchart.stock.naver.com/sise.nhn?symbol="
               ,ticker,"&timeframe=day&count=",days,"&requestType=0")

  price = GET(url) %>%
    read_html(encoding = 'EUC-KR') %>%
    html_nodes("item") %>%
    html_attr("data") %>%
    read_delim(delim = '|')

  # price = price[c(1, 5)]
  price = data.frame(price)
  colnames(price) = c('Date', 'Open', 'High', 'Low', 'Close', 'Volume')
  price[, 1] = ymd(price[, 1])
  price = tk_xts(price)

  write.csv(data.frame(price),paste0(getwd(),"/",ticker,"_price_OHLCV.csv"))
  paste('Downloading and saving of', ticker, 'data is completed')

  }
