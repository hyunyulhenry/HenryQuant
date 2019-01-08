#' Download all listed firm's ticker in KOREA Markets.
#'
#' This function will Download all listed firm's ticker, name in KOR Markets.

#' @param src Download from 'krx' or 'naver'
#'
#' @importFrom utils write.csv tail
#' @importFrom rvest html_text html_nodes html_attr
#' @importFrom xml2 read_html
#' @importFrom httr add_headers POST
#' @importFrom readr read_csv
#' @examples
#' \dontrun{
#'  ticker = get_KOR_ticker()
#'  }
#' @export
get_KOR_ticker = function(src = 'krx') {

  print("Ticker Download")
  data = list()

  # Download form krx
  if (src == 'krx') {

    down_data = function(market, date) {

      gen_otp_url = "http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx"
      gen_otp_data = list(name = "fileDown", filetype = "csv",
                          url = "MKD/04/0404/04040200/mkd04040200_01", market_gubun = market,
                          indx_ind_cd = "", sect_tp_cd = "", schdate = date, pagePath = "/contents/MKD/04/0404/04040200/MKD04040200.jsp")
      otp = POST(gen_otp_url, query = gen_otp_data)
      otp_content = read_html(otp) %>% html_text()

      down_url = "http://file.krx.co.kr/download.jspx"
      down_data = list(code = otp_content)
      down = POST(down_url, query = down_data,
                  add_headers(referer = gen_otp_url)) %>%
        read_html() %>% html_text() %>% read_csv()

      return(down)
    }

    ticker = c("STK", "KSQ")
    ticker_add = c("KS", "KQ")
    date = gsub("-", "", Sys.Date()-1)

    for (i in 1:2) {

      down_table = c()

      while (is.null(down_table)) {
        tryCatch({
          down_data(ticker[i], date)
        }, error = function(e) {
          date <<- as.character(as.numeric(date) - 1)
        })
        down_table = down_data(ticker[i], date)
      }

      down_table$market = ticker_add[i]
      down_table = down_table[, -1] %>% data.frame()
      data[[i]] = down_table

    }

    data = do.call(rbind, data)

  }

  # Download form naver
  if (src == 'naver') {

    for (i in 0:1) {

      mkt = ifelse(i == 0, "KS", "KQ")

      ticker = list()
      url = paste0("https://finance.naver.com/sise/sise_market_sum.nhn?sosok=",i,"&page=1")
      down_table = GET(url)

      navi.final = read_html(down_table, encoding = "EUC-KR") %>%
        html_nodes(".pgRR") %>%
        html_nodes("a") %>%
        html_attr("href")

      navi.final = strsplit(navi.final, "=") %>%
        unlist() %>%
        tail(1) %>%
        as.numeric()

      for (j in 1:navi.final) {

        url = paste0("https://finance.naver.com/sise/sise_market_sum.nhn?sosok=",i,"&page=",j)
        down_table = GET(url)

        Sys.setlocale("LC_ALL", "English")

        table = read_html(down_table, encoding = "EUC-KR") %>% html_table(fill = TRUE)
        table = table[[2]]

        Sys.setlocale("LC_ALL", "Korean")

        table[, ncol(table)] = NULL
        table = na.omit(table)

        symbol = read_html(down_table, encoding = "EUC-KR") %>%
          html_nodes("tbody") %>%
          html_nodes("td") %>%
          html_nodes("a") %>%
          html_attr("href")

        symbol = sapply(symbol, function(x) {
          substr(x, nchar(x) - 5, nchar(x))
        }) %>% unique()

        table$N = symbol
        colnames(table)[1] = "\uc885\ubaa9\ucf54\ub4dc"

        rownames(table) = NULL
        ticker[[j]] = table

        if ((j %% 5) == 0) {
          print( paste0(mkt, " ", round(j / navi.final * 100,  3), "%") )
        }

        Sys.sleep(0.5)
      }

      ticker = do.call(rbind, ticker)
      ticker$market = mkt

      data[[i + 1]] = ticker
      Sys.sleep(2)

    }

    data = do.call(rbind, data)
    data = data[which(data["\uc561\uba74\uac00"] != "0"), ]

    numeric.point = c(3:4, 6:12)
    data[,numeric.point] = sapply(data[,numeric.point], function(x) {
      gsub(",", "", x) %>%
        as.numeric()
    })

  }

  print("Data download is complete. Data cleansing is in progress.")

  data = data[!grepl("\uc2a4\ud329", data[, 2]), ] # 스팩 종목 제외
  data = data[substr(data[,2], nchar(data[,2]), nchar(data[,2])) != "\uc6b0", ] # 우선주 제외
  data = data[substr(data[,2], nchar(data[,2]) - 1, nchar(data[,2])) != "\uc6b0\u0042", ] # 우선주 제외
  data = data[substr(data[,2], nchar(data[,2]) - 1, nchar(data[,2])) != "\uc6b0\u0043", ] # 우선주 제외
  data = data[substr(data[,2], nchar(data[,2]), nchar(data[,2])) != "\ud638", ] # 특별 종목 제외

  rownames(data) = NULL

  write.csv(data, "KOR_ticker.csv")
  return(data)

}
