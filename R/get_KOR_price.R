#' Download all listed firm's price in Korea Markets.
#'
#' This function will Download all listed firm's Adjusted price in Korea Markets.
#' (KOSPI, KOSDAQ)
#'
#' It will aumomatically save individual stock prices and
#' combined prices for csv types
#'
#' @param src Download from 'yahoo' or 'naver'
#'
#' @importFrom utils write.csv
#' @importFrom xts xts as.xts
#' @importFrom zoo na.locf
#' @importFrom lubridate ymd today
#' @importFrom magrittr "%>%"
#' @importFrom httr GET
#' @importFrom xml2 read_html
#' @importFrom rvest html_node
#' @importFrom quantmod Cl getSymbols
#' @examples
#' \dontrun{
#'  get_KOR_price()
#'  }
#' @export
get_KOR_price = function(src = "naver") {

  folder_name = "KOR_price"
  ifelse(dir.exists(folder_name), FALSE, dir.create(folder_name))

  ticker = get_KOR_ticker()

  ticker_name = function(src) {
    if (src == "yahoo") {name = paste0(ticker[i, 1], ".", ticker[i, 'market'])}
    if (src == "naver") {name = ticker[i, 1]}
    return(name)
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

      if (src == "naver") {
        price = xts(NA, order.by = Sys.Date())

        tryCatch({
          url = paste0("https://fchart.stock.naver.com/sise.nhn?symbol="
                       ,name,"&timeframe=day&count=1000&requestType=0")

          data = GET(url) %>%
            read_html %>%
            html_nodes("item") %>%
            html_attr("data") %>%
            strsplit("\\|")

          price = do.call(rbind, data) %>% data.frame()
          price = price[c(1,5)] # 첫번째 열은 날짜, 다섯번째 열은 종가

          price[,1] = ymd(price[,1])
          price = column_to_rownames(price, var = 'X1')

          price[,1] = as.character(price[,1]) %>% as.numeric()
          price = as.xts(price)
          price = price[!duplicated(index(price))]

        }, error = function(e) {
          warning(paste0("Error in Ticker: ", name))}
        )
      }

      write.csv(data.frame(price),paste0(getwd(),"/",folder_name,"/",name,"_price.csv"))
      print(paste0(ticker[i, '\uc885\ubaa9\ucf54\ub4dc']," ",
                   ticker[i, '\uc885\ubaa9\uba85']," ",
                   round(i / nrow(ticker) * 100,3),"%"))

      Sys.sleep(3)
    }
  }

  print("Data download is complete. Data binding is in progress.")

  # Arrange #
  price_list = list()
  for (i in 1 : nrow(ticker)){
    name = ticker_name(src)
    price_list[[i]] = as.xts(read.csv(paste0(getwd(),"/",folder_name,"/",name,"_price.csv"), row.names = 1))
    if ((i %% 100) == 0) { print(paste0("Binding Price: ", round((i / nrow(ticker)) * 100,2)," %")) }
  }

  price_list = do.call(cbind, price_list)
  price_list = na.locf(price_list)
  colnames(price_list) = ticker[, '\uc885\ubaa9\ucf54\ub4dc']

  write.csv(data.frame(price_list),paste0(getwd(),"/",folder_name,".csv"))

}
