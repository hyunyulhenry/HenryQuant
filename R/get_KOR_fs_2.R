#' Download all listed firms financial statement data in korea markets
#'
#' This function will Download all listed valuaation and financial statement data,
#' usually data for the last 4 years
#'
#' get_KOR_fs is downloading data from SEJONG DATA, and
#' get_KOR_fs2 is downloading data from FNGUIDE
#'
#' @return Save financial statement data for all listed firms
#' @importFrom magrittr "%>%"
#' @importFrom stats setNames
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_node html_text html_table
#' @importFrom stringi stri_trim_both
#' @importFrom utils write.csv
#' @importFrom httr GET
#'
#' @examples
#' \dontrun{
#'   get_KOR_fs2()
#'   }
#' @export
get_KOR_fs2 = function() {

value_name = "Korea_value"
fs_name = "Korea_fs_2"
ticker = get_KOR_ticker()

ifelse(dir.exists(value_name), FALSE, dir.create(value_name))
ifelse(dir.exists(fs_name), FALSE, dir.create(fs_name))

  #--- Download VALUE ---#

  down_value = function(tick) {

    txt = data.frame(matrix(0, ncol = 5)) %>% setNames(c("PER","12M PER", "Peer PER", "PBR", "Div Yield"))

    tryCatch({
    url = paste0("http://comp.fnguide.com/SVO2/ASP/SVD_Main.asp?pGB=1&gicode=A",tick,"&cID=&MenuYn=Y&ReportGB=&NewMenuID=101&stkGb=701")
    temp = GET(url)
    temp = read_html(temp)
    temp = html_node(temp, ".corp_group2")
    txt = html_text(temp)

    txt = gsub("[\r\n\t]", " ", txt)
    txt = gsub("%", "", txt)
    txt = stri_trim_both(txt)

    txt = unlist(strsplit(txt, " "))
    txt = txt[c(35,75,121,173,218)]
    txt = data.frame(matrix(txt, ncol = 5)) %>% setNames(c("PER","12M PER", "Peer PER", "PBR", "Div Yield"))

    }, error = function(e){})
    write.csv(txt,paste0(getwd(),"/",value_name,"/",tick,"_value.csv"))
  }

  #--- Download FS ---#

  down_fs = function(tick) {

    FS_data = c()
    tryCatch({

      Sys.setlocale("LC_ALL", "English") # To English
      url = paste0("http://comp.fnguide.com/SVO2/asp/SVD_Finance.asp?pGB=1&gicode=A",tick,"&cID=&MenuYn=Y&ReportGB=D&NewMenuID=103&stkGb=701")

      temp = GET(url)
      temp = read_html(temp, encoding = "utf-8")
      BS = html_node(temp, xpath = '//*[@id="divDaechaY"]/table') %>% html_table
      IS = html_node(temp, xpath = '//*[@id="divSonikY"]/table') %>% html_table
      CF = html_node(temp, xpath = '//*[@id="divCashY"]/table') %>% html_table

      Sys.setlocale("LC_ALL", "Korean")

      IS = IS[, 1:5]
      FS_data = rbind(BS, IS, CF)
    }, error = function(e){})
    write.csv(FS_data,paste0(getwd(),"/",fs_name,"/",tick,"_fs.csv"))
  }

  #--- Download Data ---#
  for(i in 1: nrow(ticker) ) {

    tick = ticker[i,1]
    v = paste0(getwd(),"/",value_name,"/",tick,"_value.csv")
    f = paste0(getwd(),"/",fs_name,"/",tick,"_fs.csv")

    #--- Existing Test ---#
    if ((file.exists(v) == TRUE) & (file.exists(f) == TRUE)) {
      next
    }

    if ((file.exists(v) == TRUE) & (file.exists(f) == FALSE)) {
      down_fs(tick)
    }

    if ((file.exists(v) == FALSE) & (file.exists(f) == TRUE)) {
      down_value(tick)
    }

    if ((file.exists(v) == FALSE) & (file.exists(f) == FALSE)) {
      down_fs(tick)
      down_value(tick)
    }

    #--- End ---#
    print(paste0(tick," ",ticker[i,2]," ",round(i / nrow(ticker) * 100,3),"%"))
    Sys.sleep(1)
  }

  value_list = list()
  for (i in 1 : nrow(ticker)){
    value_list[[i]] = read.csv(paste0(getwd(),"/",value_name,"/",ticker[i,1],"_value.csv"), row.names = 1) %>%
      set_rownames(ticker[i, 1])
  }

  value_list = do.call(rbind, value_list)
  write.csv(value_list,paste0(getwd(),"/","value_list_KOR.csv"))

  Sys.sleep(1)

}
