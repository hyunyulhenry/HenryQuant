#' Download all listed firm's ticker in KOREA Markets.
#'
#' This function will Download all listed firm's ticker, name in KOR Markets.
#' @return ticker, name
#' @importFrom utils write.csv
#' @importFrom httr POST content
#' @importFrom RCurl getURL
#' @examples
#' \dontrun{
#'  ticker = get_KOR_ticker()
#'  }
#' @export
get_KOR_ticker = function() {

  d = function(market, date) {
    gen_otp_url = "http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx"
    gen_otp_data = list(name = "fileDown", filetype = "csv",
                         url = "MKD/04/0404/04040200/mkd04040200_01", market_gubun = market,
                         indx_ind_cd = "", sect_tp_cd = "", schdate = date, pagePath = "/contents/MKD/04/0404/04040200/MKD04040200.jsp")
    otp = POST(gen_otp_url, query = gen_otp_data)
    otp_content = content(otp, "text")
    down_url = "http://file.krx.co.kr/download.jspx"
    down_data = list(code = otp_content)
    html = POST(down_url, query = down_data)
    down = getURL(html$url, .encoding = "UTF-8")
    temp = read_csv(down)
    return(temp)
  }

  ticker = c("STK", "KSQ")
  date = gsub("-", "", Sys.Date())
  data = list()

  for (i in 1:2) {

    tryCatch({
      d(ticker[i], date)
    }, error = function(e) {
      date <<- gsub("-", "", Sys.Date()-1)
    })

    x = d(ticker[i], date)

    data[[i]] = as.data.frame(x) %>% column_to_rownames(var = colnames(x)[1])
  }

  data = do.call(rbind, data)
  data = data[, 1:2]
  Sys.setlocale("LC_ALL", "Korean")
  write.csv(data, "KOR_ticker_list.csv")
  return(data)
  }

