#' Download all listed firms valuation data in korea markets
#'
#' This function will Download all listed firms valuation data
#' (EPS, BPS, PER, PEER PER, PBR, Dividend Yield)
#'
#' @return Save valuation data for all listed firms
#'
#' @importFrom magrittr "%>%" set_rownames
#' @importFrom httr GET
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom stringi stri_trim_both
#' @importFrom utils write.csv
#'
#' @examples
#' \dontrun{
#'   get_KOR_value()
#'   }
#' @export
get_KOR_value = function() {

  folder_name = "Korea_value"
  ticker = get_KOR_ticker()

  ifelse(dir.exists(folder_name), FALSE, dir.create(folder_name))

  for(i in 1 : nrow(ticker)) {
    if(file.exists(paste0(getwd(),"/",folder_name,"/",ticker[i,1],"_",ticker[i,2],"_value.csv")) == TRUE){
      next
    } else {

      tryCatch({

        url = paste0("http://companyinfo.stock.naver.com/v1/company/c1030001.aspx?cmp_cd=",ticker[i,1])
        val = GET(url)
        val2 = read_html(val)
        tbl = html_nodes(val2, ".cmp-table-cell")
        txt = html_text(tbl)[3]
        txt = gsub("[\r\n\t]", " ", txt)
        txt = gsub(":"," ", txt)
        txt = gsub("%", "", txt)
        txt = stri_trim_both(txt)

        txt = unlist(strsplit(txt, " ")) %>% unique
        txt = txt[txt!=""]

        result = data.frame(matrix(txt, ncol = length(txt)/2), stringsAsFactors = FALSE)
        names(result) = result[1 ,]
        result = result[-1, -ncol(result)]
        rownames(result) = NULL

        write.csv(result,paste0(getwd(),"/",folder_name,"/",ticker[i,1],"_",ticker[i,2],"_value.csv"))
        print(paste0(ticker[i,2]," ",round(i / nrow(ticker) * 100,2),"%"))

      }, error = function(e){})
    }

    Sys.sleep(1)
  }

  value_list = list()
  for (i in 1:nrow(ticker)) {
    value_list[[i]] = read.csv(paste0(getwd(),"/",folder_name,"/",ticker[i,1],"_",ticker[i,2],"_value.csv"), row.names = 1) %>%
      set_rownames(ticker[i, 1])
  }
  value_list = do.call(rbind, value_list)
  write.csv(value_list, paste0(getwd(), "/", "value_list_KOR.csv"))
}




