#' Download the components of WICS standard 10 sectors.
#'
#' @param src Download from 'wics'
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select rename
#' @examples
#' \dontrun{
#'  sector = get_KOR_sector()
#'  }
#' @export
get_KOR_sector = function(src = 'wics') {

  SEC_NM_KOR = CMP_CD = CMP_KOR = NULL
  print("Download Sector Constitution Information")

  # Basic information
  sector.code = c(
    'G10',
    'G15',
    'G20',
    'G25',
    'G30',
    'G35',
    'G40',
    'G45',
    'G50',
    'G55'
  )

  # Test for trading date
  date = gsub("-", "", Sys.Date()-1)
  test.date = F

  while (test.date == F) {
    test.url = paste0(
      'http://www.wiseindex.com/Index/GetIndexComponets?ceil_yn=0&dt=',
      date,'&sec_cd=',sector.code[1])

    test.info = fromJSON(test.url)$list
    if (length(test.info) == 0) {
      date = as.character(as.numeric(date) - 1)
    } else {
      test.date = T
      }
  }

  # Download all data
  data.full = list()
  counter = 1

  for (i in sector.code) {

    url = paste0(
      'http://www.wiseindex.com/Index/GetIndexComponets?ceil_yn=0&dt=',
      date,'&sec_cd=',i)
    data = fromJSON(url)
    data.sector = data$list

    data.sector = data.sector %>%
      select(SEC_NM_KOR, CMP_CD, CMP_KOR) %>%
      rename('\uc139\ud130' = SEC_NM_KOR,
             '\uc885\ubaa9\ucf54\ub4dc' = CMP_CD,
             '\uc885\ubaa9\uba85' = CMP_KOR)

    data.full[[i]] = data.sector

    print(paste(data.sector[1,1], "/",counter / length(sector.code) * 100, "%"))
    counter = counter + 1
    Sys.sleep(2)
  }

  data.full = do.call(rbind, data.full)
  rownames(data.full) = NULL

  write.csv(data.full, "KOR_sector_components.csv")
  return(data.full)
}
