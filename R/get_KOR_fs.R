#' Download all listed firms financial statement data in korea markets
#'
#' This function will Download all listed firms financial statement data,
#' usually data for the last 20 years
#'
#' @return Save financial statement data for all listed firms

#' @importFrom magrittr "%>%"
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom stringi stri_trim_both
#' @importFrom utils write.csv
#'
#' @examples
#' \dontrun{
#'   get_KOR_fs()
#'   }
#' @export
get_KOR_fs = function() {

  folder_name = "KOR_fs"
  ticker = get_KOR_ticker()

  ifelse(dir.exists(folder_name), FALSE, dir.create(folder_name))

  for(i in 1 : nrow(ticker)) {
    if(file.exists(paste0(getwd(),"/",folder_name,"/",ticker[i,1],"_",ticker[i,2],"_fs.csv")) == TRUE){
      next
    } else {

      dataAll = c()

      tryCatch({

        for(j in 1 : 2) {
          url = paste0("http://www.sejongdata.com/business_include_fr/table_main0_bus_01.html?no=",
                       ticker[i,1],"&gubun=",j)
          temp = get_Table(url)
          temp = data.frame(country=ticker[i,2],temp)
          temp[, 2] = substr(temp[,2], 1,4)

          dataAll = rbind(dataAll,temp)
        }

        write.csv(dataAll,paste0(getwd(),"/",folder_name,"/",ticker[i,1],"_",ticker[i,2],"_fs.csv"))
        print(paste0(dataAll$country[1]," ",round(i / nrow(ticker) * 100,2),"%"))

      }, error = function(e){})
    }

    Sys.sleep(1)
  }
}
