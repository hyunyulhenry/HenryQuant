#' Download all listed firm's ticker in KOREA Markets.
#'
#' This function will Download all listed firm's ticker, name in KOR Markets.

#' @param src Download from 'krx' or 'naver'
#'
#' @importFrom utils write.csv tail
#' @importFrom rvest html_text html_nodes html_attr
#' @importFrom xml2 read_html
#' @importFrom httr add_headers POST write_disk
#' @importFrom readr read_csv
#' @examples
#' \dontrun{
#'  ticker = get_KOR_ticker()
#'  }
#' @export
get_KOR_ticker = function(src = 'krx') {

  print("Ticker Download")
  data = list()

  chr_to_numeric = function(x) {
    x = gsub(',', '', x) %>% as.numeric
    return(x)
  }

  # Download form krx
  if (src == 'krx') {

    gen_otp_url = "http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx"
    down_url = "http://file.krx.co.kr/download.jspx"

    # 산업별 현황 다운로드
    down_sector = function(date) {
      gen_otp_data = list(name = "fileDown", filetype = "csv",
                          url = "MKD/03/0303/03030103/mkd03030103", tp_cd = 'ALL',
                          date = date, lang = 'ko',
                          pagePath = "/contents/MKD/03/0303/03030103/MKD03030103.jsp")
      otp = POST(gen_otp_url, query = gen_otp_data) %>%
        read_html() %>% html_text()
      down_data = list(code = otp)

      down = POST(down_url, query = down_data,
                  add_headers(referer = gen_otp_url)) %>%
        read_html() %>% html_text() %>% read_csv()

      data_sector = down[, c(1:4, 7)]
      write.csv(data_sector, 'data_sector.csv')

      return(data_sector)

    }

    # 개별종목 가치 지표
    down_value = function(date) {
      gen_otp_data = list(name = "fileDown", filetype = "csv",
                          url = "MKD/13/1302/13020401/mkd13020401", market_gubun = 'ALL',
                          gubun = '1', schdate = date,
                          pagePath = "/contents/MKD/13/1302/13020401/MKD13020401.jsp")
      otp = POST(gen_otp_url, query = gen_otp_data) %>%
        read_html() %>% html_text()
      down_data = list(code = otp)
      down = POST(down_url, query = down_data,
                  add_headers(referer = gen_otp_url)) %>%
        read_html() %>% html_text() %>% read_csv()

      data_value = down[, c(2, 5, 7, 9, 11)]

      data_value[, c(3:4)] = sapply(data_value[, c(3:4)], chr_to_numeric)
      data_value$ROE = data_value$PBR / data_value$PER

      write.csv(data_value, 'data_value.csv')
      return(data_value)

    }

    date = get_recent_bizday()

    # while (test.date == F) {
    #   tryCatch({
    #     down_value(date)
    #     test.date = T
    #   }, error = function(e) {
    #     date <<- as.character(as.numeric(date) - 1)
    #   })
    # }

    down_table = list()
    down_table[[1]] = down_sector(date)
    down_table[[2]] = down_value(date)

    data = merge(down_table[[1]], down_table[[2]], by = '\uc885\ubaa9\ucf54\ub4dc')
    data = data[order(-data['\uc2dc\uac00\ucd1d\uc561\u0028\uc6d0\u0029']), ]

  }

  # Download form naver
  if (src == 'naver') {

    for (i in 0:1) {

      mkt = ifelse(i == 0, "\ucf54\uc2a4\ud53c", "\ucf54\uc2a4\ub2e5")

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
      ticker = ticker[, c(1,13,2,3,6,7,8,11,12)]

      data[[i + 1]] = ticker
      Sys.sleep(2)

    }

    data = do.call(rbind, data)
    data = data[which(data["\uc561\uba74\uac00"] != "0"), ]

    data[, c(4:9)] = sapply(data[, c(4:9)], chr_to_numeric)

  }

  print("Data download is complete. Data cleansing is in progress.")

  data = data[!grepl("\uc2a4\ud329", data[, 3]), ] # 스팩 종목 제외
  data = data[data[,3] != "\uace8\ub4e0\ube0c\ub9bf\uc9c0\uc774\uc548\u0035\ud638", ] # 골든브릿지이안5호 제외
  data = data[substr(data[,3], nchar(data[,3]), nchar(data[,3])) != "\uc6b0", ] # 우선주 제외
  data = data[substr(data[,3], nchar(data[,3]) - 1, nchar(data[,3])) != "\uc6b0\u0042", ] # 우선주 제외
  data = data[substr(data[,3], nchar(data[,3]) - 1, nchar(data[,3])) != "\uc6b0\u0043", ] # 우선주 제외

  rownames(data) = NULL

  write.csv(data, "KOR_ticker.csv")
  return(data)

}
