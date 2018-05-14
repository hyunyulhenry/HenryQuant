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

  value_name = "KOR_value"
  fs_name = "KOR_fs_2"
  ticker = get_KOR_ticker()

  ifelse(dir.exists(value_name), FALSE, dir.create(value_name))
  ifelse(dir.exists(fs_name), FALSE, dir.create(fs_name))

  #--- Download VALUE ---#

  down_value = function(tick) {

    #--- FORM ---#
    nm1 = c("EPS","CFP","SPS","BPS","DPS")
    nm2 = c("PER","PCR","PSR","PBR","DY")
    txt = data.frame(matrix(NA, ncol = 5)) %>% setNames(nm2) %>%
      set_rownames(tick)

    #--- Download Valuation Data and Current Price ---#
    tryCatch({

      Sys.setlocale("LC_ALL", "English")
      url = paste0("http://comp.fnguide.com/SVO2/ASP/SVD_Invest.asp?pGB=1&gicode=A",tick,"&cID=&MenuYn=Y&ReportGB=&NewMenuID=105&stkGb=701")

      temp = GET(url) %>% read_html( encoding = "utf-8") %>%
        html_table(fill = TRUE)

      temp = temp[[2]]

      url = paste0("http://comp.fnguide.com/SVO2/ASP/SVD_Main.asp?pGB=1&gicode=A",tick,"&cID=&MenuYn=Y&ReportGB=&NewMenuID=101&stkGb=701")
      z = GET(url) %>% read_html( encoding = "utf-8") %>% html_table(fill = TRUE)
      z = z[[1]][1,2]

      z = strsplit(z, "/") %>% unlist
      z = z[1]
      z = gsub(",", "", z) %>% as.numeric

      Sys.setlocale("LC_ALL", "Korean")

      temp = as.data.frame(lapply(temp, function(x) {
        gsub("\u00A0", " ", x)
      }))

    #--- Find Month (12) ---#

    last.col = which(unlist(strsplit(colnames(temp), "\\.")) == "12") / 2
    last.col = last.col[length(last.col)]

    #--- Calcaulate Valuation ---#
      for (t in 1:length(nm1)) {
        x = which(nm1[t] == substr(temp[,1], 1,3))[1]
        x = temp[x, last.col]
        x = gsub(",", "", x) %>% as.numeric

        if (x ==0) {
          x = NA
        }

        if (nm1[t] != "DPS") {
          txt[t] = z / x
        } else {
          txt[t] = x / z
        }
      }

    }, error = function(e){})
    write.csv(txt,paste0(getwd(),"/",value_name,"/",tick,"_value.csv"))
  }

  #--- Download FS ---#

  down_fs = function(tick) {

    FS_data = c()
    tryCatch({

      Sys.setlocale("LC_ALL", "English") # To English
      url = paste0("http://comp.fnguide.com/SVO2/asp/SVD_Finance.asp?pGB=1&gicode=A",tick,"&cID=&MenuYn=Y&ReportGB=D&NewMenuID=103&stkGb=701")

      temp = GET(url) %>% read_html(encoding = "utf-8") %>% html_table
      Sys.setlocale("LC_ALL", "Korean")

      IS = temp[[1]][,1:5]
      BS = temp[[3]]
      CF = temp[[5]]

      FS_data = rbind(IS, BS, CF)

      FS_data =  FS_data[!duplicated(FS_data[,1]),] %>% set_rownames(NULL)
      FS_data[FS_data == ""] = NA

      x = FS_data[,1]

      FS_data = apply(apply(FS_data[,2:ncol(FS_data)], 2, gsub, patt = ",", replace = ""), 2, as.numeric) %>%
        set_rownames(x) %>% as.data.frame
      # rownames(FS_data) = gsub(" 펼치기","",rownames(FS_data))

    }, error = function(e){})
    write.csv(FS_data, paste0(getwd(),"/",fs_name,"/",tick,"_fs.csv"))
  }

  #--- Download Data ---#
  for (i in 1:nrow(ticker) ) {

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
  for (i in 1:nrow(ticker)) {
    value_list[[i]] = read.csv(paste0(getwd(),"/",value_name,"/",ticker[i,1],"_value.csv"), row.names = 1) %>%
      set_rownames(ticker[i, 1])
  }

  value_list = do.call(rbind, value_list)
  write.csv(value_list,paste0(getwd(),"/","KOR_value_list.csv"))

}
