#' Download Factor Data of French Library
#'
#' It shows the factors that can be downloaded from the French library.
#' If you enter a number, corresponding data is downloaded.
#'
#' @importFrom readr read_csv
#' @importFrom utils unzip
#' @examples
#' \dontrun{
#'   data = french_library()
#'   }
#' @export
french_library = function() {

  library_list = library_list()

  #--- Default ---#

  print(library_list[, 1] %>% as.character)
  select_factor = readline(prompt="Which factor would you choose? ")
  select_factor = as.numeric(select_factor)

  folder_name = "Download_temp"
  ifelse(dir.exists(folder_name), FALSE, dir.create(folder_name))

  #--- Download ---#
  url = as.character(library_list[select_factor, 2])
  tempfile = paste0(getwd(),"/",folder_name,"/data.zip")
  download.file(url, tempfile, method="auto", quiet = FALSE, mode = "wb",cacheOK = TRUE)
  unzip(tempfile, exdir=paste0(getwd(),"/",folder_name))
  file.remove(paste0(getwd(),"/",folder_name,"/data.zip"))
  file_list = list.files(paste0(getwd(),"/",folder_name))


  #--- Cleansing ---#
  data = read_csv(file=paste0(getwd(),"/",folder_name,"/",file_list),
                  col_names = TRUE,
                  skip = library_list[select_factor, 3])

  cut = which(is.na(data[,ncol(data)]))[1]
  data = data[1 : (cut-1), ] %>% column_to_rownames(var = "X1")
  data[] = lapply(data, function(x) as.numeric(x))
  data = data / 100

  datestoformat = rownames(data)
  if (nchar(datestoformat[1]) == 8) {
    datestoformat = paste(substr(datestoformat,1,4),substr(datestoformat,5,6),substr(datestoformat,7,8),sep="-")
  } else {
  datestoformat = paste(substr(datestoformat,1,4),substr(datestoformat,5,6),"01",sep="-")
  }
  data = as.xts(data, order.by=as.Date(datestoformat))
  file.remove(paste0(getwd(),"/",folder_name,"/",file_list))

  print(as.character(library_list[select_factor, 1]))
  return(data)

}
