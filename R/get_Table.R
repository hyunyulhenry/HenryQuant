#'  A function for organizing the names of tickers and items needed for the
#'  get_kor_fs package.
#'
#' This function will Download all listed firms financial statement data,
#' usually data for the last 10 years
#'
#' It needs "rvest" and "stringi" package.
#' Scraps from http://www.sejongdata.com
#'
#' @param url "http://www.sejongdata.com/query/value.html"

#' @importFrom xml2 read_html read_html
#' @importFrom magrittr "%>%"
#' @importFrom rvest html_nodes html_text
#' @importFrom stringi stri_trim_both stri_trim_both
#' @importFrom utils write.csv
#'
#' @examples
#' \dontrun{
#'   get_kor_fs(folder_name = "Korea_FS", sleep = 2)
#'   }
#' @export

get_Table <- function(url=url){

  temp = read_html(url,encoding = "UTF-8")
  data = temp %>% html_nodes(".bus_board_txt1") %>% html_text
  item = temp %>% html_nodes('.bus_board_tit1') %>% html_text

  item = stri_trim_both(item)
  item = item[1:(which(nchar(item)==0)[2]-1)]
  item = gsub(" ","",item)
  item = gsub("[\u00A0]","",item)
  year = item[-1]

  num = length(data) / length(item)

  data = data.frame(t(matrix(data, nrow = num, byrow=T)),stringsAsFactors = F)
  names(data) = data[1,]
  data = data[-1,]
  data = cbind(year,data)
  return(data)
}
