#' Download all listed firm's ticker in KOREA Markets.
#'
#' This function will Download all listed firm's ticker, name in KOR Markets.
#' @return ticker, name
#' @importFrom utils write.csv
#' @importFrom httr POST content
#' @importFrom readr read_csv
#' @importFrom rvest html_text
#' @importFrom xml2 read_html
#' @examples
#' \dontrun{
#'  ticker = get_KOR_ticker()
#'  }
#' @export
get_KOR_ticker = function() {

  down_data = function(market, date) {

    gen_otp_url = "http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx"
    gen_otp_data = list(name = "fileDown", filetype = "csv",
                        url = "MKD/04/0404/04040200/mkd04040200_01", market_gubun = market,
                        indx_ind_cd = "", sect_tp_cd = "", schdate = date, pagePath = "/contents/MKD/04/0404/04040200/MKD04040200.jsp")
    otp = POST(gen_otp_url, query = gen_otp_data)
    otp_content = read_html(otp) %>% html_text()

    down_url = "http://file.krx.co.kr/download.jspx"
    down_data = list(code = otp_content)
    html = POST(down_url, query = down_data)
    down = read_html(html$url) %>% html_text()
    temp = read_csv(down)
    return(temp)
  }

  ticker = c("STK", "KSQ")
  ticker_add = c("KS", "KQ")
  date = gsub("-", "", Sys.Date()-1)
  data = list()

  for (i in 1:2) {
    down_table = c()
    while (is.null(down_table)) {
      tryCatch({
        down_data(ticker[i], date)
      }, error = function(e) {
        date <<- as.character(as.numeric(date) - 1)
      }, warning = function(e) {
        down_table <<- down_data(ticker[i], date)
      })
    }

    down_table$market = ticker_add[i]
    down_table = down_table[, -1] %>% data.frame()
    data[[i]] = down_table
  }

  data = do.call(rbind, data)

  data = data[!grepl("\uc2a4\ud329", data[, 2]), ] # 스팩 종목 제외
  data = data[substr(data[,2], nchar(data[,2]), nchar(data[,2])) != "\uc6b0", ] # 우선주 제외
  data = data[substr(data[,2], nchar(data[,2])-1, nchar(data[,2])) != "\uc6b0B", ] # 우선주 제외
  data = data[substr(data[,2], nchar(data[,2])-1, nchar(data[,2])) != "\uc6b0C", ] # 우선주 제외
  data = data[substr(data[,2], nchar(data[,2]), nchar(data[,2])) != "\ud638", ] # 특별 종목 제외

  write.csv(data, "KOR_ticker.csv")
  return(data)

}

