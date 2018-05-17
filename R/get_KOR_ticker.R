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
  otp_content = content(otp, "text")
  down_url = "http://file.krx.co.kr/download.jspx"
  down_data = list(code = otp_content)
  html = POST(down_url, query = down_data)
  down = read_html(html$url, encoding = "UTF-8") %>% html_text
  temp = read_csv(down)
  return(temp)
}

ticker = c("STK", "KSQ")
date = gsub("-", "", Sys.Date()-1)
data = list()

for (i in 1:2) {
  x = c()
  while (is.null(x)) {
    tryCatch({
      down_data(ticker[i], date)
    }, error = function(e) {
      date <<- as.character(as.numeric(date) - 1)
    }, warning = function(e) {
      x <<- down_data(ticker[i], date)
    })
  }

  data[[i]] = as.data.frame(x) %>% column_to_rownames(var = colnames(x)[1])
}

data = do.call(rbind, data)
data = data[, c(1:2, 11)]
data[,3] = data[,3] / 100000000

Sys.setlocale("LC_ALL", "Korean")
write.csv(data, "KOR_ticker_list.csv")
return(data)

}
