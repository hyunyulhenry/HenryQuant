#' Download all listed firm's price in Korea Markets.
#'
#' This function will Download all listed firm's Adjusted price in Korea Markets.
#' (KOSPI, KOSDAQ)
#'
#' It will aumomatically save individual stock prices and
#' combined prices for csv types
#'
#' @param src Download from 'yahoo' or 'daum'
#'
#' @importFrom utils write.csv
#' @importFrom xts xts as.xts
#' @importFrom zoo na.locf
#' @importFrom lubridate ymd today
#' @importFrom magrittr "%>%"
#' @importFrom httr GET
#' @importFrom xml2 read_html
#' @importFrom rvest html_node
#' @importFrom jsonlite fromJSON
#' @importFrom quantmod Cl getSymbols
#' @examples
#' \dontrun{
#'  get_KOR_price()
#'  }
#' @export
get_KOR_price = function(src = "daum") {

  folder_name = "KOR_price"
  ifelse(dir.exists(folder_name), FALSE, dir.create(folder_name))

  ticker = get_KOR_ticker()

  ticker_name = function(src) {
    if (src == "yahoo") {name = paste0(ticker[i, 1], ".", ticker[i, 'market'])}
    if (src == "daum") {name = ticker[i, 1]}
    return(name)
  }

  # Cleansing #
  data_cleansing  = function(data) {

    data_table = cbind(data$date, data$tradePrice) %>% data.frame()

    data_table[,2] = data_table[,2] %>% as.character() %>% as.numeric()
    rownames(data_table) = data_table[,1]
    data_table[,1] = NULL

    data_table = as.xts(data_table)
    data_table = data_table[!duplicated(index(data_table))]

    return(data_table)

  }

  for(i in 1 : nrow(ticker) ) {
    name = ticker_name(src)

    if(file.exists(paste0(folder_name,"/",name,"_price.csv")) == TRUE){
      next
    } else {

      if (src == "yahoo") {
        price = xts(NA, order.by = Sys.Date())

        tryCatch({
          price = Cl(getSymbols(name, auto.assign = FALSE))
        }, error = function(e) {
          print(paste0("Error in Ticker: ", name))
        })
      }

      if (src == "daum") {
        price = xts(NA, order.by = Sys.Date())

        tryCatch({
          url = paste0("http://finance.daum.net/api/charts/A",name,
                       "/days?limit=1000&adjusted=true")

          data = fromJSON(url)
          data = data[[1]]
          price = data_cleansing(data)

        }, error = function(e) {
          warning(paste0("Error in Ticker: ", name))}
        )
      }

      write.csv(as.matrix(price),paste0(getwd(),"/",folder_name,"/",name,"_price.csv"))
      print(paste0(ticker[i, 1]," ",ticker[i,2]," ",round(i / nrow(ticker) * 100,3),"%"))

      Sys.sleep(3)
    }
  }

  print("Data download is complete. Data binding is in progress.")

  # Arrange #
  price_list = list()
  for (i in 1 : nrow(ticker)){
    name = ticker_name(src)
    price_list[[i]] = as.xts(read.csv(paste0(getwd(),"/",folder_name,"/",name,"_price.csv"), row.names = 1))
    if ((i %% 10) == 0) { print(paste0("Binding Price: ", round((i / nrow(ticker)) * 100,2)," %")) }
  }

  price_list = do.call(cbind, price_list)
  price_list = na.locf(price_list)
  colnames(price_list) = ticker[, 1]

  write.csv(data.frame(price_list),paste0(getwd(),"/",folder_name,".csv"))

}
