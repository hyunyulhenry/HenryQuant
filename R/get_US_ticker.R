#' Download all listed firm's ticker in US Markets.
#'
#' This function will Download all listed firm's ticker, name, and sector in US Markets.
#' (NYSE, NASDAQ, AMEX Market)

#' @return ticker, name, sector
#' @importFrom utils download.file read.csv
#' @examples
#' \dontrun{
#'  get_US_ticker()
#'  }
#' @export

get_US_ticker = function() {

  url_NASDAQ = "http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nasdaq&render=download"
  url_NYSE = "http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=nyse&render=download"
  url_AMEX = "http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=amexe&render=download"

  download.file(url_NASDAQ, destfile = "./url_NASDAQ.csv")
  download.file(url_NYSE, destfile = "./url_NYSE.csv")
  download.file(url_AMEX, destfile = "./url_AMEX.csv")

  NASDAQ = read.csv("./url_NASDAQ.csv", stringsAsFactors = F)
  NYSE = read.csv("./url_NYSE.csv", stringsAsFactors = F)
  AMEX = read.csv("./url_AMEX.csv", stringsAsFactors = F)

  temp = rbind(NASDAQ, NYSE, AMEX)
  temp = temp[(temp$Sector != "n/a") & (temp$MarketCap != "n/a") ,]

  temp = temp[!duplicated(temp[,2]), ]
  temp$Symbol = gsub(" ", "", temp$Symbol)

  result = cbind(temp$Symbol, temp$Name, temp$Sector)
  write.csv(result, "ticker_list.csv")

  return(result)
}

