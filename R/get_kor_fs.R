#' Download all listed firms financial statement data in korea markets
#'
#' This function will Download all listed firms financial statement data,
#' usually data for the last 10 years
#'
#' It needs "rvest" and "stringi" package.
#' Scraps from http://www.sejongdata.com
#'
#' @param folder_name Where you want the downloaded information to be stored
#' @param sleep Sleep time when downloading data
#' @return Save financial statement data for all listed firms

#' @importFrom magrittr "%>%"
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom stringi stri_trim_both
#' @importFrom utils write.csv
#'
#' @examples
#' \dontrun{
#'   get_kor_fs(folder_name = "KoreaFS", sleep = 2)
#'   }
#' @export

get_kor_fs = function(folder_name = "KoreaFS", sleep = 1) {

  url = "http://www.sejongdata.com/query/value.html"

  temp = xml2::read_html(url,encoding = "UTF-8")
  data = temp %>% html_nodes(".bus_board_txt1") %>% html_text
  item = temp %>% html_nodes('.bus_board_tit1') %>% html_text

  Data_scrap = data.frame(matrix(data ,ncol=5, byrow=T))
  names(Data_scrap) = item
  Data_scrap = Data_scrap[,c(1,2)]
  rm(data, item, temp, url)

  ifelse(dir.exists(folder_name), FALSE, dir.create(folder_name))

  for(i in 1 : nrow(Data_scrap)) {

    dataAll = c()

    tryCatch({

      for(j in 1 : 2) {
        url = paste0("http://www.sejongdata.com/business_include_fr/table_main0_bus_01.html?no=",
                     Data_scrap[i,1],"&gubun=",j)
        temp = get_Table(url)
        temp = data.frame(country=Data_scrap[i,2],temp)
        temp[, 2] = substr(temp[,2], 1,4)
        ro = nrow(temp)

        dataAll = rbind(dataAll,temp)
      }

      write.csv(dataAll,paste0(getwd(),"/",folder_name,"/",Data_scrap[i,1],"_",Data_scrap[i,2],".csv"))
      print(paste0(dataAll$country[1]," ",round(i / 2314 * 100,3),"%"))

    }, error = function(e){})
    Sys.sleep(sleep)
  }
}

