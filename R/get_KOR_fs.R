#' Download all listed firms financial statement data in korea markets
#'
#' This function will Download all listed valuation and financial statement data,
#' usually data for the last 4 years
#'
#' Downloading data from Naver finance (https://companyinfo.stock.naver.com)
#'
#' @return Save financial statement & and valuation data for all listed firms
#' @importFrom magrittr "%>%"
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_node html_text html_table
#' @importFrom utils write.csv
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom data.table rbindlist
#'
#' @examples
#' \dontrun{
#'   get_KOR_fs()
#'   }
#' @export
get_KOR_fs = function() {

  value_name = "KOR_value"
  fs_name = "KOR_fs"
  ticker = get_KOR_ticker()

  ifelse(dir.exists(value_name), FALSE, dir.create(value_name))
  ifelse(dir.exists(fs_name), FALSE, dir.create(fs_name))

  # Download VALUE #

  down_value = function(name) {

    data_value = c()

    tryCatch({

      url = paste0("https://companyinfo.stock.naver.com/v1/company/cF4002.aspx?cmp_cd=",
                   name,"&frq=0&rpt=5&finGubun=MAIN&frqTyp=0&cn=")
      Sys.setlocale("LC_ALL", "English")
      data = fromJSON(url)
      data = data[[2]] %>% data.frame()
      Sys.setlocale("LC_ALL", "Korean")

      data_table = cbind(data$DATA5)
      rownames(data_table) = data$ACC_NM

      value.type = c("EPS", "BPS", "CPS", "SPS", "DPS")
      data_value = data_table[sapply(value.type, function(x) {which(rownames(data_table) == x)}), ]

      url = paste0("https://companyinfo.stock.naver.com/v1/company/c1010001.aspx?cmp_cd=",
                   name,"&cn=")
      price = GET(url) %>%
        read_html() %>%
        html_nodes(xpath = '//*[@id="cTB11"]/tbody/tr[1]/td/strong') %>%
        html_text()

      price = gsub(",", "", price) %>% as.numeric()
      data_value = price / data_value
      data_value[5] = 1/data_value[5]
      names(data_value) = c("PER", "PBR", "PCR", "PSR", "DY")

      data_value[is.infinite(data_value)] = NA

    }, error = function(e) {
      data_value <<- NA
      warning(paste0("Error in Ticker: ", name))}
    )

    write.csv(data_value, paste0(getwd(),"/",value_name,"/",name,"_value.csv"))
  }


  # Download FS #
  down_fs = function(name) {
    data_fs = list()
    tryCatch({

      for (j in 0 : 2) {
        url = paste0("https://companyinfo.stock.naver.com/v1/company/cF3002.aspx?cmp_cd=",
                     name,"&frq=0&rpt=",j,"&finGubun=MAIN&frqTyp=0&cn=")
        Sys.setlocale("LC_ALL", "English")
        data = fromJSON(url)
        Sys.setlocale("LC_ALL", "Korean")

        yr_name = data[[1]][1:5] %>% data.frame()
        yr_name = apply(yr_name, 1, function(x) {substr(x, 1, 4)})

        data = data[[2]]
        data_table = cbind(data$DATA1, data$DATA2, data$DATA3, data$DATA4, data$DATA5)
        rownames(data_table) = data$ACC_NM
        colnames(data_table) = yr_name

        data_fs[[j+1]] = data_table
        Sys.sleep(0.5)
      }

      data_fs = do.call(rbind, data_fs)
      data_fs = data_fs[!duplicated(rownames(data_fs)), ]

    }, error = function(e) {
      data_fs <<- NA
      warning(paste0("Error in Ticker: ", name))}
    )
    write.csv(data_fs, paste0(getwd(),"/",fs_name,"/",name,"_fs.csv"))
  }

  # Download Data #
  for (i in 1:nrow(ticker) ) {

    name = ticker[i,1]
    v = paste0(getwd(),"/",value_name,"/",name,"_value.csv")
    f = paste0(getwd(),"/",fs_name,"/",name,"_fs.csv")

    # Existing Test #
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

  item = data_value[[1]] %>% rownames()
  value_list = list()
  temp_data = c()

  for (i in 1 : length(item)) {
    value_list[[i]] = lapply(data_value, function(x) {
      if ( item[i] %in% rownames(x) ) {
        x[which(rownames(x) == item[i]),]
      } else {
        NA
      }
    })
  }

  value_list = lapply(value_list, function(x) {do.call(rbind, x)})
  value_list = do.call(cbind, value_list) %>% data.frame()

  rownames(value_list) = ticker[, 1]
  colnames(value_list) = item

  write.csv(value_list, "KOR_value.csv")


  # Binding Financial Statement #
  print("Binding Financial Statement")
  data_fs = list()
  for (i in 1 : nrow(ticker)) {
    name = ticker[i, 1] %>% as.character()
    data_fs[[i]] = read.csv(paste0(getwd(),"/",fs_name,"/",name,"_fs.csv"), row.names = 1)
  }

  item = data_fs[[1]] %>% rownames()
  fs_list = list()
  temp_data = c()

  for (i in 1 : length(item)) {
    fs_list[[i]] = lapply(data_fs, function(x) {
      if ( item[i] %in% rownames(x) ) {
        x[which(rownames(x) == item[i]),]
      } else {
        matrix(NA, 1, 5) %>% data.frame()
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
  saveRDS(fs_list, paste0("KOR_fs.Rds"))

}
