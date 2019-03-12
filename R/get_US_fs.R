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

  # Download Data (From Yahoo) #
  down_fs = function(name) {

    # Download Financial Statement #
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

      rownames(data_fs) = NULL
      data_fs = column_to_rownames(data_fs, var = names(data_fs)[1])

      colnames(data_fs) = data_fs[1,]
      data_fs = data_fs[-1, ]

      data_fs = data_fs[, substr(colnames(data_fs), 1,2) == "12"]

      for (j in 1:ncol(data_fs)) {
        data_fs[, j] = str_replace_all(data_fs[,j], ',', '') %>% as.numeric()
      }

      colnames(data_fs) = sapply(colnames(data_fs), function(x) {
        substr(x,nchar(x)-3, nchar(x))
      })

    }, error = function(e) {
      data_fs <<- NA
      print(paste0("Error in Ticker: ", name))}
    )

    # Download Valuation Data #
    data_value = c()
    Ratios = yahooQF(c("Previous Close", "Shares Outstanding"))

    tryCatch({
      data_inform = getQuote(name, what = Ratios)[-1] %>% as.numeric()
      value.type = c("Net Income Applicable To Common Shares", # Earnings
                     "Total Stockholder Equity", # Book Value
                     "Total Cash Flow From Operating Activities", # Cash Flow
                     "Total Revenue")

      data_value = data_fs[match(value.type, rownames(data_fs)), 1] * 1000
      data_value = data_inform[1] / ( data_value / data_inform[2])
      names(data_value) = c("PER", "PBR", "PCR", "PSR")

    }, error = function(e) {
      data_value <<- NA
      print(paste0("Error in Ticker: ", name))}
    )

    write.csv(data_fs, paste0(getwd(),"/",fs_name,"/",name,"_fs.csv"))
    write.csv(data_value, paste0(getwd(),"/",value_name,"/",name,"_value.csv"))
  }

  # Download Data #
  for(i in 1: nrow(ticker) ) {

    name = ticker[i, 1]
    f = paste0(getwd(),"/",fs_name,"/",name,"_fs.csv")
    v = paste0(getwd(),"/",value_name,"/",name,"_value.csv")

    # Existing Test #
    if ( (file.exists(f) == TRUE) & (file.exists(v) == TRUE) ) {
      next
    } else {
      down_fs(name)
    }

    # End #
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

  item = data_value[[1]] %>% rownames()
  value_list = list()

  for (i in 1 : length(item)) {
    value_list[[i]] = lapply(data_value, function(x) {
      if ( item[i] %in% rownames(x) ) {
        x[which(rownames(x) == item[i]), ]
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
