#' Get Strength of Momentum
#'
#' It calculate strength of Momentum based on Monthly Return
#' The comparison value is set to 0 by default and can be changed to the desired value. (Ex: risk free rate)
#'
#' @param R Return Data (Recommend using 12-month return)
#' @param Comparison The value to be the comparison value'
#' @examples
#' \dontrun{
#'   R = asset_data
#'   R = R["2000"]
#'   st_value = get_mom_strength(R, 0)
#'   }
#' @export
get_mom_strength = function(R, Comparison = 0) {

  R_m = apply.monthly(R, Return.cumulative)
  str_table = list()

  for (j in 3 : nrow(R_m)) {

    temp = R_m[nrow(R_m) : (nrow(R_m)-j+1), ] %>% Return.cumulative
    str_table[[j]] = as.integer(temp > Comparison)

  }

  str_table = do.call(rbind, str_table)
  str_result = apply(str_table, 2, mean)
  names(str_result) = colnames(R_m)

  return(str_result)
}
