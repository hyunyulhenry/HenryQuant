#'Download all listed firm's price in US Markets.
#'
#' This function will Download all listed firm's price in US Markets.
#' (NYSE, NASDAQ, AMEX Market)
#'
#' If the source is "yahoo", it will download Adjsted price,
#' and if source is "google", it will download normal price
#'
#' It will aumomatically save individual stock prices and
#' combined prices for csv types

#' @param src "yahoo" or "google"

#' @return stock prices
#' @importFrom quantmod getSymbols Ad Cl
#' @importFrom utils write.csv
#' @importFrom xts as.xts
#' @importFrom zoo na.locf
#' @examples
#' \dontrun{
#'  US_price = get_US_price(src = "yahoo")
#'  }
#' @export

get_US_price = function(src = "yahoo") {

  ifelse(dir.exists("US_price"), FALSE, dir.create("US_price"))
  ticker = get_US_ticker()

  for(i in 1: nrow(ticker) ) {

  if(file.exists(paste0(getwd(),"/","US_price","/",ticker[i,1],".csv")) == TRUE){
    next
  } else {

    tryCatch({

      price = getSymbols(ticker[i, 1], src=src, auto.assign = FALSE)
      if (src == "yahoo") {
        price = Ad(price)
        colnames(price) = unlist(strsplit(names(price), ".Adjusted"))
      } else if (src == "google") {
        price = Cl(price)
        colnames(price) = unlist(strsplit(names(price), ".Close"))
      }
      price = na.locf(price)

      write.csv(as.matrix(price),paste0(getwd(),"/","US_price","/",ticker[i,1],".csv"))
      print(paste0(ticker[i, 1]," ",ticker[i,2]," ",round(i / nrow(ticker) * 100,3),"%"))

    }, error = function(e){})

  }
    Sys.sleep(1)
  }

  price_list = list()
  for (i in 1 : nrow(ticker)){
    tryCatch({
    price_list[[i]] = as.xts(read.csv(paste0(getwd(),"/","US_price","/",ticker[i,1],".csv"), row.names = 1), drop.time = TRUE)
    }, error = function(e){})
  }

  price_list = do.call(cbind, price_list)
  price_list = na.locf(price_list)

  write.csv(as.matrix(price_list),paste0(getwd(),"/","US_price_list",".csv"))
  return(price_list)

}

