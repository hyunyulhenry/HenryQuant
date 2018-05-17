#' Download all listed firm's financial statement data in US Markets.
#' This function will Download all listed firm's financial statement data in US Markets. (NYSE, NASDAQ, AMEX Market)
#' It will aumomatically save individual stock value, financial statement
#' and combined value for csv types
#' @return stock value and financial statement data
#' @importFrom utils write.csv
#' @importFrom quantmod getFinancials getQuote viewFinancials yahooQF
#' @examples
#' \dontrun{
#'  get_US_fs()
#'  }
#' @export
get_US_fs = function() {

  value_name = "US_value"
  fs_name = "US_fs"

  ifelse(dir.exists(value_name), FALSE, dir.create(value_name))
  ifelse(dir.exists(fs_name), FALSE, dir.create(fs_name))

  ticker = get_US_ticker()
  Ratios = yahooQF(c("P/E Ratio", "Price/Book", "Dividend Yield"))

  #--- Download VALUE ---#

  down_value = function(tick) {

    temp_value = matrix(NA, 1, 3)
    rownames(temp_value) = tick
    colnames(temp_value) = c("P/E Ratio", "Price/Book", "Dividend Yield")

    tryCatch({
      temp_value = getQuote(tick,what=Ratios)[, -1]
    }, error = function(e){})

    write.csv(temp_value,paste0(getwd(),"/",value_name,"/",tick,"_value.csv"))
  }

  #--- FS Cleansing ---#

  fs_clean = function(data) {

    K = which((data[,1] != data[,ncol(data)]) )
    return(K)
  }

  #--- Download FS ---#

  down_fs = function(tick) {

    FS_data = c()
    tryCatch({

      Sys.setlocale("LC_ALL", "English") # To English

      IS = paste0("https://finance.yahoo.com/quote/",tick,"/financials?p=",tick) %>%
        GET %>% read_html(encoding = "utf-8") %>% html_table

      BS = paste0("https://finance.yahoo.com/quote/",tick,"/balance-sheet?p=",tick) %>%
        GET %>% read_html(encoding = "utf-8") %>% html_table

      CF = paste0("https://finance.yahoo.com/quote/",tick,"/cash-flow?p=",tick) %>%
        GET %>% read_html(encoding = "utf-8") %>% html_table

      IS = IS[[2]]
      BS = BS[[2]]
      CF = CF[[2]]

      FS_data = rbind(IS, BS, CF)
      FS_data = set_colnames(FS_data, FS_data[1,])
      FS_data = FS_data[-1, ]

      FS_data = FS_data[fs_clean(FS_data), ]
      FS_data = FS_data[!duplicated(FS_data[,1]),] %>% set_rownames(NULL)

      x = FS_data[,1]

      FS_data = apply(apply(FS_data[,2:ncol(FS_data)], 2, gsub, patt = ",", replace = ""), 2, as.numeric) %>%
        set_rownames(x) %>% data.frame

    }, error = function(e){})
    write.csv(FS_data, paste0(getwd(),"/",fs_name,"/",tick,"_fs.csv"))
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
  write.csv(value_list,paste0(getwd(),"/","US_value_list.csv"))

}

