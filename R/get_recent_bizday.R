#' Get Recent Business day in Korea Stock Market
#'
#' @importFrom stringr str_match str_replace_all
#' @examples
#' \dontrun{
#'  bizday = get_recent_bizday()
#'  }
#' @export
get_recent_bizday = function() {

  url = 'https://finance.naver.com/sise/sise_deposit.nhn'

  biz_day = GET(url) %>%
    read_html(encoding = 'EUC-KR') %>%
    html_nodes(xpath = '//*[@id="type_1"]/div/ul[2]/li/span') %>%
    html_text() %>%
    str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
    str_replace_all('\\.', '')

  return(biz_day)
}
