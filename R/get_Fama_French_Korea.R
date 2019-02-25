#' Download the Fama-French 5 Factor and Momentum Factor data.
#'
#' Initially, you need to authenticate your identity to connect google drive and API.
#' For more information on generating factor indices, see the following link:
#' https://docs.google.com/document/d/11A1NH8dy38T4Ay5UDL-ReCCx9VqV3UujeF-imx0ABu4/edit
#'
#' @param period 'daily', 'monthly', or 'yearly'
#'
#' @importFrom googledrive drive_download as_id
#' @importFrom magrittr set_colnames set_names
#' @importFrom tibble column_to_rownames
#' @examples
#' \dontrun{
#'  get_Fama_French_Korea()
#'  }
#' @export
get_Fama_French_Korea = function(period = 'daily') {

  # id (daily)
  link_id_daily = rbind.data.frame(
      c('five_factor_daily', '1phGP57z5QW4VjhAXZSMAvY6EYFbuCMc5'),
      c('momentum_daily', '1ICsw0Ar-egopA8PvjvlXAtD8W063mklO'),
      c('size_BtoM_daily', '1yO4GysY0o_SLsraeOr2rK3oXT_7Lo1pE'),
      c('size_investment_daily', '1jd5cm6tp6cl3p3f6BkZx4XoAgvh0PT-u'),
      c('size_profitability_daily', '1X6032Q20Vp2KfZK3qcHtoZ_YeB4Bmgbi'),
      c('size_momentum_daily', '1udkVYv5ok0oF8m0FG42kN-mF727Ds_CL'),
      stringsAsFactors = FALSE
      ) %>%
    set_colnames(c('factor name', 'ID'))

  # id (monthly)
  link_id_monthly = rbind.data.frame(
    c('five_factor_monthly', '126uXHX3h_0SaREUKtw4JPhUrTdrHUzpe'),
    c('momentum_monthly', '1PhxiTy7vHmB5pLqOKZJQQq5EjQN44uzj'),
    c('size_BtoM_monthly', '1IWlBbsYrI5zw-oNCFrd4HcRxJR1DRC2J'),
    c('size_investment_monthly', '1S4lbn87sj6xIq6CwxPWSxmAv3NUaJzL3'),
    c('size_profitability_monthly', '1AlAgeoMcadTFDrZLsGcTxiCJTr7duT_I'),
    c('size_momentum_monthly', '1PhxiTy7vHmB5pLqOKZJQQq5EjQN44uzj'),
    stringsAsFactors = FALSE
  ) %>%
    set_colnames(c('factor name', 'ID'))

  # id (yearly)
  link_id_yearly = rbind.data.frame(
    c('five_factor_yearly', '1prMPguzvm3rVqqUN5RA8otcrPR5PgNGX'),
    c('momentum_yearly', '17dMOMgrmyPybXHV1rnDtm8v4c-cXDU-D'),
    c('size_BtoM_yearly', '1-sbiY7cqK59yr0HrRg72PHi7opcpcLMq'),
    c('size_investment_yearly', '1ErPW93LBd9on7eRFCqTf8-Iy3XVACf2X'),
    c('size_profitability_yearly', '1aJpnmiM7RmQg6nW-HzILgx31egrtZMIk'),
    c('size_momentum_yearly', '1cUcBVyub0kHBwqPOempZC0R6UmWI0Lg9'),
    stringsAsFactors = FALSE
  ) %>%
    set_colnames(c('factor name', 'ID'))

  df = switch(period,
         daily = { link_id_daily },
         monthly = { link_id_monthly },
         yearly = { link_id_yearly }
  )

  down_factor = function(df) {
  # download data
    data =
      # download
      lapply(df$ID, function(x) {
        temp = tempfile(fileext = ".csv")
        drive_download(
          as_id(x), path = temp, overwrite = TRUE)
        Sys.sleep(1.5)
        read.csv(temp, stringsAsFactors = FALSE)}) %>%
      # set names for each factor
      set_names(df$`factor name`) %>%
      # change to xts type
      lapply(function(z) {
        column_to_rownames(z, var = 'X') %>%
          as.xts()
        })
  }

  down_data = down_factor(df)
  return(down_data)

}
