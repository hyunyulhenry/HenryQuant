#' Download the value index of Korean stock market from Korea Exchange.
#'
#' @param market KRX, KOSPI, KOSDAQ
#' @param value PBR, PER, DY
#'
#' @return PBR, PER, and Div Yield indicators of KRX, KOSPI, and KOSDAQ markets.
#' @importFrom magrittr "%>%"
#' @importFrom xml2 read_html
#' @importFrom rvest html_text
#' @importFrom httr GET
#' @importFrom xts as.xts
#' @importFrom readr read_csv
#'
#' @examples
#' \dontrun{
#'   KOR_mkt_value(market = "KOSPI", value = "PBR")
#'   }
#' @export

KOR_mkt_value = function(market = "KOSPI", value = "PBR") {

  # Market: KRX, KOSPI, KOSDAQ
  # Value : PBR, PER, DY

  if (market == "KRX") { market_type = "_01" }
  if (market == "KOSPI") { market_type = "_02" }
  if (market == "KOSDAQ") { market_type = "_03" }

  if (value == "PER") { value_type = "03" }
  if (value == "PBR") { value_type = "04" }
  if (value == "DY") { value_type = "05" }

  url = paste0('MKD/13/1301/130101',value_type,'/mkd130101',value_type,market_type)
  path = paste0('/contents/MKD/13/1301/130101',value_type,'/MKD130101',value_type,'.jsp')

  OTP_form = POST("http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx",
                  query=list(
                    name = 'filedown',
                    filetype = 'csv',
                    url = url,
                    type = market,
                    period_selector = 'day',
                    fromdate = '20080101',
                    todate = gsub("-", "", Sys.Date()-1),
                    pagePath = path)
  )
  OTP = content(OTP_form, "text")

  data = POST("http://file.krx.co.kr/download.jspx",
              query = list(
                code = OTP
              )
  )

  data_value = read_html(data$url, encoding = "UTF-8") %>% html_text
  df.value =  data.frame(read_csv(data_value))

  rownames(df.value) = df.value[,1]
  df.value[,1] = NULL
  df.value = as.xts(df.value)

  return(df.value)
}


