#' Download listed firm's ticker in JAPAN(TOPIX) Markets.
#'
#' This function will Download listed firm's ticker, name, and sector in JAPAN Markets.
#' 
#' @return listed company data
#' @importFrom readxl read_xlsx 
#' @examples
#' \dontrun{
#'  get_JAPAN_ticker()
#'  }
#' @export
get_JAPAN_ticker = function() {

  url = "https://www.jpx.co.jp/english/markets/indices/topix/tvdivq00000030ne-att/TOPIX_weight_en.xlsx"
  download.file(url, destfile = "./topix.xlsx", mode="wb")
  TOPIX = read_xlsx("./topix.xlsx")
  TOPIX = data.frame(TOPIX[!is.na(TOPIX[,1]), ])
  
  write.csv(TOPIX, "JAPAN_ticker.csv")
  file.remove("./topix.xlsx")
  
  return(TOPIX)

}