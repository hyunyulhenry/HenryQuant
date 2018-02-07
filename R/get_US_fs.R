#' Download all listed firm's financial statement data in US Markets.
#'
#' This function will Download all listed firm's financial statement data in US Markets.
#' (NYSE, NASDAQ, AMEX Market)
#'
#' It will aumomatically save individual stock value, financial statement
#' and combined value for csv types

#' @return stock value and financial statement data
#' @importFrom utils write.csv
#' @importFrom quantmod getFinancials getQuote viewFinancials yahooQF
#' @examples
#' \dontrun{
#'  US_fs = get_US_fs()
#'  }
#' @export
get_US_fs = function() {

  value_name = "US_value"
  fs_name = "US_fs"

  ifelse(dir.exists(value_name), FALSE, dir.create(value_name))
  ifelse(dir.exists(fs_name), FALSE, dir.create(fs_name))

  ticker = get_US_ticker()
  Ratios = yahooQF(c("P/E Ratio", "Price/Book", "Dividend Yield"))

  for(i in 1: nrow(ticker) ) {

    #--- Download Value ---#
    if(file.exists(paste0(getwd(),"/",value_name,"/",ticker[i,1],"_value.csv")) == TRUE) {
      print(paste0("Already Value of ",ticker[i,1]," is downloaded"))
    } else {
      temp_value = matrix(NA, 1, 3)
      rownames(temp_value) = ticker[i, 1]
      colnames(temp_value) = c("P/E Ratio", "Price/Book", "Dividend Yield")

      tryCatch({
      temp_value = getQuote(ticker[i, 1],what=Ratios)[, -1]
      }, error = function(e){})

      write.csv(temp_value,paste0(getwd(),"/",value_name,"/",ticker[i,1],"_value.csv"))
    }

    #--- Downlad Financial Statement ---#
    if(file.exists(paste0(getwd(),"/",fs_name,"/",ticker[i,1],"_fs.csv")) == TRUE){
      print(paste0("Already FS of ",ticker[i,1]," is downloaded"))
    } else {
      temp_FS = temp_BS = temp_IS = temp_CF = FS_data = c()

      tryCatch({
      temp_FS = getFinancials(ticker[i, 1], auto.assign = "FALSE")

      temp_BS = viewFinancials(temp_FS, type = 'BS', period = 'A')
      temp_IS = viewFinancials(temp_FS, type = 'IS', period = 'A')
      temp_CF = viewFinancials(temp_FS, type = 'CF', period = 'A')

      FS_data = rbind(temp_BS, temp_IS, temp_CF)

      }, error = function(e){})

      write.csv(FS_data,paste0(getwd(),"/",fs_name,"/",ticker[i,1],"_fs.csv"))
    }

    #--- End ---#
    print(paste0(ticker[i, 1]," ",ticker[i,2]," ",round(i / nrow(ticker) * 100,3),"%"))
    Sys.sleep(1)
  }

  value_list = list()
  for (i in 1 : nrow(ticker)){
    tryCatch({
      value_list[[i]] = read.csv(paste0(getwd(),"/",value_name,"/",ticker[i,1],"_value.csv"), row.names = 1)
    }, error = function(e){})
  }

  value_list = do.call(rbind, value_list)

  write.csv(value_list,paste0(getwd(),"/","value_list.csv"))

}

