#' Download all listed firm's financial statement data in US Markets.
#'
#' This function will Download all listed firm's financial statement data in US Markets. (NYSE, NASDAQ, AMEX Market)
#' It will aumomatically save individual stock value, financial statement
#' and combined data for csv & Rds types
#'
#' @return stock value and financial statement data
#' @importFrom utils write.csv
#' @importFrom magrittr "%>%"
#' @importFrom quantmod getQuote yahooQF
#' @importFrom data.table rbindlist
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

  down_value = function(name) {

    data_value = c()

    tryCatch({
      Ratios = yahooQF(c("P/E Ratio", "Price/Book", "Dividend Yield"))
      data_value = getQuote(name, what = Ratios)[-1]

    }, error = function(e) {
      data_value <<- NA
      print(paste0("Error in Ticker: ", name))}
    )

    write.csv(data_value,paste0(getwd(),"/",value_name,"/",name,"_value.csv"))

  }


  #--- Download FS ---#

  down_fs = function(name) {

    data_fs = c()

    tryCatch({
      Sys.setlocale("LC_ALL", "English")
      yahoo.finance.xpath = '//*[@id="Col1-1-Financials-Proxy"]/section/div[3]/table'

      IS = paste0("https://finance.yahoo.com/quote/",name,"/financials?p=",name) %>%
        GET() %>% read_html() %>% html_nodes(xpath = yahoo.finance.xpath) %>%
        html_table() %>% data.frame()
      Sys.sleep(0.5)

      BS = paste0("https://finance.yahoo.com/quote/",name,"/balance-sheet?p=",name) %>%
        GET() %>% read_html() %>% html_nodes(xpath = yahoo.finance.xpath) %>%
        html_table() %>% data.frame()
      Sys.sleep(0.5)

      CF = paste0("https://finance.yahoo.com/quote/",name,"/cash-flow?p=",name) %>%
        GET() %>% read_html() %>% html_nodes(xpath = yahoo.finance.xpath) %>%
        html_table() %>% data.frame()

      data_fs = rbind(IS, BS, CF)
      data_fs = data_fs[!duplicated(data_fs[, 1]), ]

      colnames(data_fs) = data_fs[1,]
      data_fs = data_fs[-1, ]

      rownames(data_fs) = data_fs[,1]
      data_fs = data_fs[,-1]

      for (j in 1:ncol(data_fs)) {
        data_fs[, j] = gsub(",", "", data_fs[, j]) %>% as.numeric
      }

      colnames(data_fs) = sapply(colnames(data_fs), function(x) {
        substring(x,nchar(x)-3, nchar(x))
      })

    }, error = function(e) {
      data_fs <<- NA
      print(paste0("Error in Ticker: ", name))}
    )

    write.csv(data_fs, paste0(getwd(),"/",fs_name,"/",name,"_fs.csv"))
  }


  #--- Download Data ---#
  for(i in 1: nrow(ticker) ) {

    name = ticker[i,1]
    v = paste0(getwd(),"/",value_name,"/",name,"_value.csv")
    f = paste0(getwd(),"/",fs_name,"/",name,"_fs.csv")

    #--- Existing Test ---#
    if ((file.exists(v) == TRUE) & (file.exists(f) == TRUE)) {
      next
    }

    if ((file.exists(v) == TRUE) & (file.exists(f) == FALSE)) {
      down_fs(name)
    }

    if ((file.exists(v) == FALSE) & (file.exists(f) == TRUE)) {
      down_value(name)
    }

    if ((file.exists(v) == FALSE) & (file.exists(f) == FALSE)) {
      down_fs(name)
      down_value(name)
    }

    #--- End ---#
    print(paste0(name," ",ticker[i,2]," ",round(i / nrow(ticker) * 100,3),"%"))
    Sys.sleep(3)
  }

  print("Data download is complete. Data binding is in progress.")

  # Binding Value #
  print("Binding Value")
  data_value = list()
  for (i in 1 : nrow(ticker)) {
    name = ticker[i, 1]
    data_value[[i]] = read.csv(paste0(getwd(),"/",value_name,"/",name,"_value.csv"), row.names = 1)
  }

  item = data_value[[1]] %>% colnames()
  value_list = list()
  temp_data = c()

  for (i in 1 : length(item)) {
    value_list[[i]] = lapply(data_value, function(x) {
      if ( item[i] %in% colnames(x) ) {
        x[which(colnames(x) == item[i])]
      } else {
        NA
      }
    })
  }

  value_list = lapply(value_list, function(x) {do.call(rbind, x)})
  value_list = do.call(cbind, value_list) %>% data.frame()

  rownames(value_list) = ticker[, 1]
  colnames(value_list) = item

  write.csv(value_list,paste0(getwd(),"/",value_name,".csv"))

  # Binding Financial Statement
  print("Binding Financial Statement")
  data_fs = list()
  for (i in 1 : nrow(ticker)) {
    name = ticker[i, 1]
    data_fs[[i]] = read.csv(paste0(getwd(),"/",fs_name,"/",name,"_fs.csv"), row.names = 1)
  }

  item = data_fs[[1]] %>% rownames()
  fs_list = list()
  temp_data = c()

  for (i in 1 : length(item)) {
    fs_list[[i]] = lapply(data_fs, function(x) {
      if ( item[i] %in% rownames(x) ) {
        cbind(x[which(rownames(x) == item[i]),],
              matrix(NA, 1, 4 - ncol(x)) %>% data.frame())
      } else {
        matrix(NA, 1, 4) %>% data.frame()
      }
    })

    if ((i %% 10) == 0) { print(paste0("Binding Financial Statement: ", round((i / length(item)) * 100,2)," %")) }
  }

  fs_list = lapply(fs_list, function(x) {rbindlist(x) %>% data.frame()})
  fs_list = lapply(fs_list, function(x) {
    rownames(x) = ticker[,1] %>% as.character()
    return(x)
  })
  names(fs_list) = item
  saveRDS(fs_list, paste0(getwd(),"/",fs_name,".Rds"))

}
