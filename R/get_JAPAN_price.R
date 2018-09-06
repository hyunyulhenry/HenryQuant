#' Download all listed firm's price in JAPAN Markets.
#'
#' This function will Download all listed firm's price in JAPAN Markets. (TOPIX)
#' It will aumomatically save individual stock prices and
#' combined prices for csv types
#'
#' @return stock prices
#' @importFrom quantmod getSymbols Ad Cl
#' @importFrom utils write.csv
#' @importFrom xts as.xts
#' @importFrom zoo na.locf
#' @examples
#' \dontrun{
#'  get_JAPAN_price()
#'  }
#' @export
get_JAPAN_price = function() {

  folder_name = "JAPAN_price"
  ticker = get_JAPAN_ticker()

  ifelse(dir.exists(folder_name), FALSE, dir.create(folder_name))

  # Price Download #
  for(i in 1 : nrow(ticker) ) {

    name = paste0(ticker[i, 3],".T")

    if(file.exists(paste0(getwd(),"/",folder_name,"/",name,"_price.csv")) == TRUE){
      next
    } else {

      price = xts(NA, order.by = Sys.Date())

      tryCatch({
        price = Cl(getSymbols(name, auto.assign = FALSE))
        colnames(price) = unlist(strsplit(names(price), ".Close"))
      }, error = function(e) {
        print(paste0("Error in Ticker: ", name))
      })

      price = price[!duplicated(index(price))]

      write.csv(as.matrix(price),paste0(getwd(),"/",folder_name,"/",name,"_price.csv"))
      print(paste0(name," ",ticker[i,2]," ",round(i / nrow(ticker) * 100,3),"%"))

      Sys.sleep(3)

    }
  }

  print("Data download is complete. Data binding is in progress.")

  # Arrange #

  price_list = list()
  for (i in 1 : nrow(ticker)) {

    name = paste0(ticker[i, 3],".T")
    price_list[[i]] = as.xts(read.csv(paste0(getwd(),"/",folder_name,"/",name,"_price.csv"), row.names = 1))
    if ((i %% 10) == 0) { print(paste0("Binding Price: ", round((i / nrow(ticker)) * 100,2)," %")) }
  }

  price_list = do.call(cbind, price_list)
  price_list = price_list[!duplicated(index(price_list))]
  price_list = na.locf(price_list)
  colnames(price_list) = paste0(ticker[, 3],".T")

  write.csv(data.frame(price_list),paste0(getwd(),"/",folder_name,".csv"))

}
