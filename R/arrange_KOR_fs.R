#' Arrange all listed firm's financial statement data in US Markets.
#'
#' You should be execute get_US_fs function first to download financial statement data.
#' It will arrange fs data by account for list type, and save csv file.
#' @return arranged financial statement data by account
#' @importFrom utils read.csv write.csv
#' @importFrom magrittr "%>%" set_colnames set_rownames
#' @importFrom tibble column_to_rownames
#' @importFrom dplyr bind_rows
#' @examples
#' \dontrun{
#'  KOR_fs = arrange_KOR_fs()
#'  }
#' @export
arrange_KOR_fs = function() {

  fs_name = "KOR_fs_2"
  if (dir.exists(fs_name) == FALSE) {
    stop("You Need Download Financial Statement Data. Please execute get_KOR_fs2() first")
  }

  ticker = get_KOR_ticker()

  test = read.csv(paste0(getwd(),"/",fs_name,"/",ticker[1,1],"_fs.csv"), row.names = 1)
  m = nrow(test)
  n = ncol(test)
  fs_colnames = c("FY -3", "FY -2", "FY -1", "FY  0")
  fs_rownames = rownames(test)

  fs_list = rep( list(list()), m )

  for (i in 1 : nrow(ticker)) {

    temp = matrix(NA, m, n) %>% as.data.frame %>% set_colnames(fs_colnames) %>% set_rownames(fs_rownames)
    tryCatch({
      temp = read.csv(paste0(getwd(),"/",fs_name,"/",ticker[i,1],"_fs.csv"), row.names = 1, stringsAsFactors = FALSE)
    }, error = function(e){})

    temp2 = matrix(NA, m, n) %>% as.data.frame %>% set_colnames(fs_colnames) %>% set_rownames(fs_rownames)
    x = which(rownames(temp) %in% fs_rownames)

    for (t in 1 : length(x)) {
      temp2[which(fs_rownames == rownames(temp[x[t], ])), ] = temp[x[t],]
    }

    for (j in 1 : m) {
      if (i == 1) {
        fs_list[[j]] = temp2[j, ]
      } else {
        fs_list[[j]] = bind_rows(fs_list[[j]], temp2[j, ])
      }

    }

    if ((i %% 50) == 0) { print(paste0(round((i / nrow(ticker)) * 100,2)," %")) }
  }

  fs_list = lapply(fs_list, function(x) {row.names(x) = ticker[,1]; x})
  names(fs_list) = fs_rownames
  write.csv(fs_list, "KOR_fs_list.csv")
  return(fs_list)
}
