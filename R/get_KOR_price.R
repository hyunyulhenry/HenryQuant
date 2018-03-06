#' Download all listed firm's price in Korea Markets.
#'
#' This function will Download all listed firm's Adjusted price in Korea Markets.
#' (KOSPI, KOSDAQ)
#'
#' It will aumomatically save individual stock prices and
#' combined prices for csv types
#'
#' @param num_limit number of days to download. 1 for 30 days
#'
#' @importFrom utils write.csv
#' @importFrom xts as.xts
#' @importFrom zoo na.locf
#' @importFrom lubridate ymd today
#' @importFrom tibble column_to_rownames
#' @importFrom magrittr "%>%" set_rownames
#' @importFrom httr GET
#' @importFrom xml2 read_html
#' @importFrom rvest html_node
#' @examples
#' \dontrun{
#'  get_KOR_price(num_limit = 25)
#'  }
#' @export
get_KOR_price = function(num_limit = 25) {

  folder_name = "KOR_price"
  ticker = get_KOR_ticker()

  ifelse(dir.exists(folder_name), FALSE, dir.create(folder_name))

  #--- Cleansing Function ---#

  cleansing = function(temp) {

    temp[temp == ""] = NA
    temp = na.omit(temp)[, c(1,5)]
    rownames(temp) = NULL
    temp = column_to_rownames(temp, var = colnames(temp)[1])

    rownames(temp) = gsub('\\.',"-",rownames(temp))
    x = rownames(temp)
    x = ymd(x) %>% as.character
    temp = apply(apply(temp, 2, gsub, patt=",", replace=""), 2, as.numeric) %>% set_rownames(x)

    return(temp)
  }

  #--- Crawling ---#

  for (i in 1 : nrow(ticker)) {

    if(file.exists(paste0(getwd(),"/","KOR_price","/",ticker[i,1],"_price.csv")) == TRUE){
      next
    } else {

    tryCatch({

    price = list()

    for (j in 1 : num_limit) {

      Sys.setlocale("LC_ALL", "English")
      url = paste0("http://finance.daum.net/item/quote_yyyymmdd_sub.daum?page=",j,"&code=",ticker[i, 1],"&modify=1")
      temp = GET(url)
      temp = read_html(temp, encoding = "utf-8")
      temp = html_node(temp, xpath = '//*[@id="bbsList"]') %>% html_table

      if (nrow(temp) <= 2) {
        next
      }

      price[[j]] = cleansing(temp)

    }

    price = do.call(rbind, price) %>% as.xts %>% set_colnames(ticker[i, 1])
    price = price[!duplicated(index(price)), ]

    }, error = function(e){})
    }

    # If there is no data or crawling error, make it as empty price

    tryCatch({

    if (length(price) == 0) {
      price = xts(matrix(NA, 1, 1), order.by = today())
      warning(paste0("Check Ticker"," ",ticker[i, 1]))
    }

    if (ncol(price) >=2) {
      price = xts(matrix(NA, 1, 1), order.by = today())
      warning(paste0("Check Ticker"," ",ticker[i, 1]))
    }

    }, error = function(e){})

    write.csv(as.matrix(price),paste0(getwd(),"/","KOR_price","/",ticker[i,1],"_price.csv"))
    print(paste0(ticker[i, 1]," ",ticker[i,2]," ",round(i / nrow(ticker) * 100,3),"%"))

    Sys.sleep(1)
  }

  #--- Arrange ---#
  price_list = list()
  for (i in 1 : nrow(ticker)){
    price_list[[i]] = as.xts(read.csv(paste0(getwd(),"/","KOR_price","/",ticker[i,1],"_price.csv"), row.names = 1))
  }

  price_list = do.call(cbind, price_list)
  price_list = na.locf(price_list)

  write.csv(as.matrix(price_list),paste0(getwd(),"/","KOR_price_list",".csv"))

}
