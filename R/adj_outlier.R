#' Corrects data located in outlier.
#'
#' It is possible to use the
#' Windsorizing method to correct the data above the reference value and the
#' Trim(Truncation) method to delete data above the reference value.
#'
#' @param Input Factor Exposure Data
#' @param Target reference value, basically 1 percent
#' @param Winsorizing TRUE for Winsorizing method, FALSE for Trim(Truncation) method
#' @importFrom stats quantile
#'
#' @examples
#' \dontrun{
#' Input = sample(-1000:1000, 100, replace=F)
#' x = adj_outlier(Input, Target = 0.01, Winsorizing = TRUE)
#'  }
#' @export
adj_outlier = function(Input, Target = 0.01, Winsorizing = TRUE) {

  Input = as.numeric(Input)

  l_num = quantile(Input, Target, na.rm = TRUE)
  u_num = quantile(Input, (1-Target), na.rm = TRUE)

  if (Winsorizing == TRUE) {
    Input[which(Input < l_num)] = l_num
    Input[which(Input > u_num)] = u_num
  } else {
    Input[which(Input < l_num)] = NA
    Input[which(Input > u_num)] = NA
  }

  return(Input)

}
