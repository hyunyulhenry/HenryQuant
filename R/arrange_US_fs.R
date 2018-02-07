#' Arrange all listed firm's financial statement data in US Markets.
#' You should be execute get_US_fs function first to download financial statement data.
#'
#' It will arrange fs data by account for list type, and save csv file.
#' @return arranged financial statement data by account
#' @importFrom utils read.csv write.csv
#' @importFrom magrittr "%>%" set_colnames set_rownames
#' @importFrom tibble column_to_rownames
#' @examples
#' \dontrun{
#'  US_fs = arrange_US_fs()
#'  }
#' @export
arrange_US_fs = function() {

  ticker = get_US_ticker()
  fs_name = "US_fs"
  test = read.csv(paste0(getwd(),"/",fs_name,"/",ticker[1,1],"_fs.csv"))
  m = nrow(test) - 1
  n = ncol(test) - 1
  fs_colnames = paste0("Past ",seq(n)," Yr")
  fs_account = unique(test[,1]) %>% as.character

  fs_list = rep( list(list()), m )

  if (dir.exists(fs_name) == FALSE) {
    warning("You Need Download Financial Statement Data. Please execute get_US_fs() first")
    break
  } else {

    for (i in 1 : nrow(ticker)) {
      tryCatch({
        temp = matrix(NA, m, n) %>% as.data.frame %>% set_colnames(fs_colnames)
        temp = read.csv(paste0(getwd(),"/",fs_name,"/",ticker[i,1],"_fs.csv"))
        temp = temp[!duplicated(temp[,1]),] %>% set_rownames(NULL) %>%
          column_to_rownames(var = "X")
        if (ncol(temp) != 4) {
          temp = cbind(temp, matrix(NA, nrow(temp), (4-ncol(temp)) ))
        }
        colnames(temp) = fs_colnames
      }, error = function(e){})

    for (j in 1 : m) {
      if (i == 1) {
        fs_list[[j]] = temp[j, ] %>% set_rownames(ticker[i, 1])
        } else {
        fs_list[[j]] = rbind(fs_list[[j]], temp[j, ] %>% set_rownames(ticker[i, 1]))
        }
      }

      if ((i %% 50) == 0) { print(paste0(round((i / nrow(ticker)) * 100,2)," %")) }
    }
    names(fs_list) = fs_account
    write.csv(fs_list, "fs_list_US.csv")
    return(fs_list)
  }

}
