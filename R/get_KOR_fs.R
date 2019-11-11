#' Download all listed firms financial statement data in korea markets
#'
#' This function will Download all listed firm's financial statement data and
#' valuation data
#'
#' Downloading data from Yahoo Finance or Company Guide
#'
#' @return Save financial statement & and valuation data for all listed firms
#' @importFrom magrittr "%>%"
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_node html_text html_table
#' @importFrom utils write.csv
#' @importFrom httr GET
#' @importFrom stringr str_replace_all
#' @importFrom dplyr bind_rows
#' @importFrom tibble column_to_rownames
#'
#' @param src Download from fn or yahoo
#'
#' @examples
#' \dontrun{
#'   get_KOR_fs()
#'   }
#' @export

get_KOR_fs = function(src = "fn") {

  value_name = "KOR_value"
  fs_name = "KOR_fs"

  ifelse(dir.exists(value_name), FALSE, dir.create(value_name))
  ifelse(dir.exists(fs_name), FALSE, dir.create(fs_name))

  ticker = get_KOR_ticker()

  ticker_name = function(src) {
    if (src == "yahoo") {name = paste0(ticker[i, 1], ".", ticker[i, 'market'])}
    if (src == "fn") {name = ticker[i, 1]}
    return(name)
  }

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

      Sys.setlocale("LC_ALL", "Korean")

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

    # Download Valuation Data #
    data_value = c()
    Ratios = yahooQF(c("Previous Close", "Shares Outstanding"))

    tryCatch({
      data_inform = getQuote(name, what = Ratios)[-1] %>% as.numeric()
      value.type = c("Net Income Applicable To Common Shares", # Earnings
                     "Total Stockholder Equity", # Book Value
                     "Total Cash Flow From Operating Activities", # Cash Flow
                     "Total Revenue", # Sales
                     "Dividends Paid") # Div Yield

      data_value = data_fs[sapply(value.type, function(x) {which(rownames(data_fs) == x)}), 1] * 1000
      data_value = data_inform[1] / ( data_value / data_inform[2])
      data_value[5] = -(1 / data_value[5])
      names(data_value) = c("PER", "PBR", "PCR", "PSR", "Div")

    }, error = function(e) {
      data_value <<- NA
      print(paste0("Error in Ticker: ", name))}
    )

    write.csv(data_fs, paste0(getwd(),"/",fs_name,"/",name,"_fs.csv"))
    write.csv(data_value, paste0(getwd(),"/",value_name,"/",name,"_value.csv"))
  }

  # Download FS (From FnGuide) #
  down_fs_fn = function(name) {

    # Download Financial Statement #
    data_fs = c()
    tryCatch({

      url = paste0("http://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A",
                   name)

      Sys.setlocale("LC_ALL", "English")
      data = GET(url)

      data_IS = data %>%
        read_html() %>%
        html_node(xpath = '//*[@id="divSonikY"]/table') %>%
        html_table()
      data_IS = data_IS[, 1:(ncol(data_IS)-2)]

      data_BS = data %>%
        read_html() %>%
        html_node(xpath = '//*[@id="divDaechaY"]/table') %>%
        html_table()

      data_CF = data %>%
        read_html() %>%
        html_node(xpath = '//*[@id="divCashY"]/table') %>%
        html_table()

      Sys.setlocale("LC_ALL", "Korean")

      data_fs = rbind(data_IS, data_BS, data_CF)
      data_fs[,1] = gsub("\uacc4\uc0b0\uc5d0 \ucc38\uc5ec\ud55c \uacc4\uc815 \ud3bc\uce58\uae30",
                         "", data_fs[,1])
      data_fs = data_fs[!duplicated(data_fs[,1]), ]
      rownames(data_fs) = NULL
      data_fs = column_to_rownames(data_fs, var = names(data_fs)[1])

      data_fs = data_fs[, substr(colnames(data_fs), 6,7) == "12"]

      for (j in 1:ncol(data_fs)) {
        data_fs[,j] = str_replace_all(data_fs[,j], ',', '') %>% as.numeric()
      }

    }, error = function(e) {
      data_fs <<- NA
      warning(paste0("Error in Ticker: ", name))}
    )

    # Download Valuation Data #
    data_value = c()
    tryCatch({
      value.type = c("\uc9c0\ubc30\uc8fc\uc8fc\uc21c\uc774\uc775", # Earnings
                     "\uc790\ubcf8", # Book Value
                     "\uc601\uc5c5\ud65c\ub3d9\uc73c\ub85c\uc778\ud55c\ud604\uae08\ud750\ub984", # Operating Cash Flow
                     "\ub9e4\ucd9c\uc561")

      value_index = data_fs[match(value.type, rownames(data_fs)), ncol(data_fs)]

      url = paste0("http://comp.fnguide.com/SVO2/ASP/SVD_Main.asp?pGB=1&gicode=A",
                   name)
      data = GET(url)

      price = data %>%
        read_html() %>%
        html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
        html_text() %>%
        str_replace_all(',', '') %>%
        as.numeric()

      share = data %>%
        read_html() %>%
        html_node(xpath = '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
        html_text() %>%
        strsplit('/') %>%
        unlist()

      share = share[1] %>%
        str_replace_all(',', '') %>%
        as.numeric()

      data_value = price / (value_index * 100000000/ share)
      names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')

      data_value[is.infinite(data_value)] = NA
      data_value[data_value < 0] = NA

    }, error = function(e) {
      data_value <<- NA
      warning(paste0("Error in Ticker: ", name))}
    )

    write.csv(data_fs, paste0(getwd(),"/",fs_name,"/",name,"_fs.csv"))
    write.csv(data_value, paste0(getwd(),"/",value_name,"/",name,"_value.csv"))
  }


  # Download Data #
  for(i in 1: nrow(ticker) ) {

    name = ticker_name(src)
    f = paste0(getwd(),"/",fs_name,"/",name,"_fs.csv")
    v = paste0(getwd(),"/",value_name,"/",name,"_value.csv")

    # Existing Test #
    if ( (file.exists(f) == TRUE) & (file.exists(v) == TRUE) ) {
      next
    } else {
      if (src == "yahoo") {down_fs(name)}
      if (src == "fn") {down_fs_fn(name)}
    }

    # End #
    print(paste0(name," ",ticker[i, '\uc885\ubaa9\uba85']," ",round(i / nrow(ticker) * 100,3),"%"))
    Sys.sleep(3)
  }

  print("Data download is complete. Data binding is in progress.")

  # Binding Value #
  print("Binding Value")
  data_value = list()
  for (i in 1 : nrow(ticker)) {
    name = ticker_name(src)
    data_value[[i]] = read.csv(paste0(getwd(),"/",value_name,"/",name,"_value.csv"), row.names = 1) %>%
      t() %>% data.frame()
  }

  data_value = bind_rows(data_value)
  value_list = data_value[colnames(data_value) != 'X1']

  rownames(value_list) = ticker[, '\uc885\ubaa9\ucf54\ub4dc']
  write.csv(value_list,paste0(getwd(),"/",value_name,".csv"))

  # Binding Financial Statement
  print("Binding Financial Statement")
  data_fs = list()
  for (i in 1 : nrow(ticker)) {
    name = ticker_name(src)
    data_fs[[i]] = read.csv(paste0(getwd(),"/",fs_name,"/",name,"_fs.csv"), row.names = 1)
  }

  item = data_fs[[1]] %>% rownames()
  fs_list = list()

  for (i in 1 : length(item)) {
    fs_list[[i]] = lapply(data_fs, function(x) {
      if ( item[i] %in% rownames(x) ) {
        x[which(rownames(x) == item[i]), ]
      } else {
        data.frame(NA)
      }
    })

    if ((i %% 10) == 0) { print(paste0("Binding Financial Statement: ", round((i / length(item)) * 100,2)," %")) }
  }

  fs_list = lapply(fs_list, function(x) {

    fs_bind = bind_rows(x)
    fs_bind = fs_bind[colnames(fs_bind) != 'NA.']
    rownames(fs_bind) = ticker[, '\uc885\ubaa9\ucf54\ub4dc']
    return(fs_bind)

  })

  names(fs_list) = item
  saveRDS(fs_list, paste0(getwd(),"/",fs_name,".Rds"))

}
