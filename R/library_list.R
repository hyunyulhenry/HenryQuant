#' List of French Library Data
#'
#' Data From French Library: http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
#'
#' @examples
#' \dontrun{
#'   list = library_list()
#'   }
#' @export
library_list = function()  {

  library_list = data.frame("NAME"= character(0), "URL"= character(0), "SKIP" = numeric(0))

  library_list =
    rbind(library_list,
          data.frame("NAME" = "FF 3 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip", "SKIP" = 3),
          data.frame("NAME" = "FF 5 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip", "SKIP" = 3),

          data.frame("NAME" = "Portfolio Formed on Size","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_ME_CSV.zip", "SKIP" = 12),
          data.frame("NAME" = "Portfolio Formed on BtoM","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_BE-ME_CSV.zip", "SKIP" = 23),
          data.frame("NAME" = "Portfolio Formed on Operating Profitability","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_OP_CSV.zip", "SKIP" = 18),
          data.frame("NAME" = "Portfolio Formed on Investment","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_INV_CSV.zip", "SKIP" = 17),

          data.frame("NAME" = "6 Portfolio Formed on Size & BtoM (2 X 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_2x3_CSV.zip", "SKIP" = 15),
          data.frame("NAME" = "25 Portfolio Formed on Size & BtoM (5 X 5)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_5x5_CSV.zip", "SKIP" = 15),
          data.frame("NAME" = "100 Portfolio Formed on Size & BtoM (10 X 10)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/100_Portfolios_10x10_CSV.zip", "SKIP" = 15),

          data.frame("NAME" = "6 Portfolio Formed on Size & Operating Profitability (2 X 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_OP_2x3_CSV.zip", "SKIP" = 16),
          data.frame("NAME" = "25 Portfolio Formed on Size & Operating Profitability (5 X 5)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_OP_5x5_CSV.zip", "SKIP" = 16),
          data.frame("NAME" = "100 Portfolio Formed on Size & Operating Profitability (10 X 10)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/100_Portfolios_ME_OP_10x10_CSV.zip", "SKIP" = 16),

          data.frame("NAME" = "6 Portfolio Formed on Size & Investment (2 X 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_INV_2x3_CSV.zip", "SKIP" = 16),
          data.frame("NAME" = "25 Portfolio Formed on Size & Investment (5 X 5)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_INV_5x5_CSV.zip", "SKIP" = 16),
          data.frame("NAME" = "100 Portfolio Formed on Size & Investment (10 X 10)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/100_Portfolios_ME_INV_10x10_CSV.zip", "SKIP" = 16),

          data.frame("NAME" = "25 Portfolio Formed on BtoM & Operating Profitability (5 X 5)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_BEME_OP_5x5_CSV.zip", "SKIP" = 15),
          data.frame("NAME" = "25 Portfolio Formed on BtoM & Investment (5 X 5)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_BEME_INV_5x5_CSV.zip", "SKIP" = 15),
          data.frame("NAME" = "25 Portfolio Formed on Operating Profitability & Investment (5 X 5)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_OP_INV_5x5_CSV.zip", "SKIP" = 18),

          data.frame("NAME" = "32 Portfolio Formed on Size, BtoM, Operating Profitability (2 X 4 X 4)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/32_Portfolios_ME_BEME_OP_2x4x4_CSV.zip", "SKIP" = 17),
          data.frame("NAME" = "32 Portfolio Formed on Size, BtoM, Investment (2 X 4 X 4)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/32_Portfolios_ME_BEME_INV_2x4x4_CSV.zip", "SKIP" = 17),
          data.frame("NAME" = "32 Portfolio Formed on Size, Operating Profitability, Investment (2 X 4 X 4)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/32_Portfolios_ME_OP_INV_2x4x4_CSV.zip", "SKIP" = 18),

          data.frame("NAME" = "Portfolio Formed on Earnings / Price","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_E-P_CSV.zip", "SKIP" = 17),
          data.frame("NAME" = "Portfolio Formed on Cashflow / Price","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_CF-P_CSV.zip", "SKIP" = 18),
          data.frame("NAME" = "Portfolio Formed on Dividend Yield","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_D-P_CSV.zip", "SKIP" = 19),

          data.frame("NAME" = "Momentum","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip", "SKIP" = 13),
          data.frame("NAME" = "6 Portfolio Formed on Size & Momentum (2 X 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_Prior_12_2_CSV.zip", "SKIP" = 11),
          data.frame("NAME" = "25 Portfolio Formed on Size & Momentum (5 X 5)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_Prior_12_2_CSV.zip", "SKIP" = 11),
          data.frame("NAME" = "10 Portfolio Formed on Momentum","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/10_Portfolios_Prior_12_2_CSV.zip", "SKIP" = 10),

          data.frame("NAME" = "Short Term Reversal","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_ST_Reversal_Factor_CSV.zip", "SKIP" = 13),
          data.frame("NAME" = "6 Portfolio Formed on Size & Short Term Reversal (2 X 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_Prior_1_0_CSV.zip", "SKIP" = 11),
          data.frame("NAME" = "25 Portfolio Formed on Size & Short Term Reversal (5 X 5)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_Prior_1_0_CSV.zip", "SKIP" = 11),
          data.frame("NAME" = "10 Portfolio Formed on Short Term Reversal","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/10_Portfolios_Prior_1_0_CSV.zip", "SKIP" = 10),

          data.frame("NAME" = "Long Term Reversal","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_LT_Reversal_Factor_CSV.zip", "SKIP" = 13),
          data.frame("NAME" = "6 Portfolio Formed on Size & Long Term Reversal (2 X 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/6_Portfolios_ME_Prior_60_13_CSV.zip", "SKIP" = 11),
          data.frame("NAME" = "25 Portfolio Formed on Size & Long Term Reversal (5 X 5)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_Prior_60_13_CSV.zip", "SKIP" = 11),
          data.frame("NAME" = "10 Portfolio Formed on Long Term Reversal","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/10_Portfolios_Prior_60_13_CSV.zip", "SKIP" = 10),

          data.frame("NAME" = "Accruals","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_AC_CSV.zip", "SKIP" = 17),
          data.frame("NAME" = "25 Portfolio Formed on Size & Accruals (5 X 5)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_AC_5x5_CSV.zip", "SKIP" = 18),

          data.frame("NAME" = "Market Beta","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_BETA_CSV.zip", "SKIP" = 15),
          data.frame("NAME" = "25 Portfolio Formed on Size & Market Beta","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_BETA_5x5_CSV.zip", "SKIP" = 16),

          data.frame("NAME" = "Net Share Issue","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_NI_CSV.zip", "SKIP" = 16),
          data.frame("NAME" = "25 Portfolio Formed on Size & Net Share Issue","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_NI_5x5_CSV.zip", "SKIP" = 18),

          data.frame("NAME" = "Variance","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_VAR_CSV.zip", "SKIP" = 16),
          data.frame("NAME" = "25 Portfolio Formed on Size & Variance","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_VAR_5x5_CSV.zip", "SKIP" = 19),

          data.frame("NAME" = "Residual Variance","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_RESVAR_CSV.zip", "SKIP" = 16),
          data.frame("NAME" = "25 Portfolio Formed on Size & Residual Variance","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_RESVAR_5x5_CSV.zip", "SKIP" = 19),

          data.frame("NAME" = "Global FF 3 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_3_Factors_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "Global ex US FF 3 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_ex_US_3_Factors_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "European FF 3 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Europe_3_Factors_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "Japanese FF 3 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Japan_3_Factors_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "Asia Pacific ex Japan FF 3 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Asia_Pacific_ex_Japan_3_Factors_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "North American FF 3 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/North_America_3_Factors_CSV.zip", "SKIP" = 6),

          data.frame("NAME" = "Global FF 5 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_5_Factors_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "Global ex US FF 5 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_ex_US_5_Factors_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "European FF 5 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Europe_5_Factors_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "Japanese FF 5 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Japan_5_Factors_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "Asia Pacific ex Japan FF 5 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Asia_Pacific_ex_Japan_5_Factors_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "North American FF 5 factor","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/North_America_5_Factors_CSV.zip", "SKIP" = 6),

          data.frame("NAME" = "Global Momentum","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_Mom_Factor_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "Global ex US Momentum","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_ex_US_Mom_Factor_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "European Momentum","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Europe_Mom_Factor_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "Japanese Momentum","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Japan_Mom_Factor_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "Asia Pacific ex Japan Momentum","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Asia_Pacific_ex_Japan_MOM_Factor_CSV.zip", "SKIP" = 6),
          data.frame("NAME" = "North American Momentum","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/North_America_Mom_Factor_CSV.zip", "SKIP" = 6),

          data.frame("NAME" = "6 Global Portfolio Formed on Size & BtoM (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_6_Portfolios_ME_BE-ME_CSV.zip", "SKIP" = 20),
          data.frame("NAME" = "6 Global ex US Portfolio Formed on Size & BtoM (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_ex_US_6_Portfolios_ME_BE-ME_CSV.zip", "SKIP" = 20),
          data.frame("NAME" = "6 Global European Portfolio Formed on Size & BtoM (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Europe_6_Portfolios_ME_BE-ME_CSV.zip", "SKIP" = 20),
          data.frame("NAME" = "6 Global Japanese Portfolio Formed on Size & BtoM (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Japan_6_Portfolios_ME_BE-ME_CSV.zip", "SKIP" = 20),
          data.frame("NAME" = "6 Global Asia Pacific ex Japan Portfolio Formed on Size & BtoM (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_BE-ME_CSV.zip", "SKIP" = 20),
          data.frame("NAME" = "6 Global North American Portfolio Formed on Size & BtoM (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/North_America_6_Portfolios_ME_BE-ME_CSV.zip", "SKIP" = 20),

          data.frame("NAME" = "6 Global Portfolio Formed on Size & Operating Profitability (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_6_Portfolios_ME_OP_CSV.zip", "SKIP" = 20),
          data.frame("NAME" = "6 Global ex US Portfolio Formed on Size & Operating Profitability (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_ex_US_6_Portfolios_ME_OP_CSV.zip", "SKIP" = 20),
          data.frame("NAME" = "6 Global European Portfolio Formed on Size & Operating Profitability (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Europe_6_Portfolios_ME_OP_CSV.zip", "SKIP" = 20),
          data.frame("NAME" = "6 Global Japanese Portfolio Formed on Size & Operating Profitability (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Japan_6_Portfolios_ME_OP_CSV.zip", "SKIP" = 20),
          data.frame("NAME" = "6 Global Asia Pacific ex Japan Portfolio Formed on Size & Operating Profitability (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_OP_CSV.zip", "SKIP" = 20),
          data.frame("NAME" = "6 Global North American Portfolio Formed on Size & Operating Profitability (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/North_America_6_Portfolios_ME_OP_CSV.zip", "SKIP" = 20),

          data.frame("NAME" = "6 Global Portfolio Formed on Size & Investment (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_6_Portfolios_ME_INV_CSV.zip", "SKIP" = 18),
          data.frame("NAME" = "6 Global ex US Portfolio Formed on Size & Investment (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_ex_US_6_Portfolios_ME_INV_CSV.zip", "SKIP" = 18),
          data.frame("NAME" = "6 Global European Portfolio Formed on Size & Investment (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Europe_6_Portfolios_ME_INV_CSV.zip", "SKIP" = 18),
          data.frame("NAME" = "6 Global Japanese Portfolio Formed on Size & Investment (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Japan_6_Portfolios_ME_INV_CSV.zip", "SKIP" = 18),
          data.frame("NAME" = "6 Global Asia Pacific ex Japan Portfolio Formed on Size & Investment (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_INV_CSV.zip", "SKIP" = 18),
          data.frame("NAME" = "6 Global North American Portfolio Formed on Size & Investment (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/North_America_6_Portfolios_ME_INV_CSV.zip", "SKIP" = 18),

          data.frame("NAME" = "6 Global Portfolio Formed on Size & Momentum (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_6_Portfolios_ME_Prior_12_2_CSV.zip", "SKIP" = 12),
          data.frame("NAME" = "6 Global ex US Portfolio Formed on Size & Momentum (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Global_ex_US_6_Portfolios_ME_Prior_12_2_CSV.zip", "SKIP" = 12),
          data.frame("NAME" = "6 Global European Portfolio Formed on Size & Momentum (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Europe_6_Portfolios_ME_Prior_12_2_CSV.zip", "SKIP" = 12),
          data.frame("NAME" = "6 Global Japanese Portfolio Formed on Size & Momentum (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Japan_6_Portfolios_ME_Prior_12_2_CSV.zip", "SKIP" = 12),
          data.frame("NAME" = "6 Global Asia Pacific ex Japan Portfolio Formed on Size & Momentum (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Asia_Pacific_ex_Japan_6_Portfolios_ME_Prior_12_2_CSV.zip", "SKIP" = 12),
          data.frame("NAME" = "6 Global North American Portfolio Formed on Size & Momentum (2 x 3)","URL" = "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/North_America_6_Portfolios_ME_Prior_12_2_CSV.zip", "SKIP" = 12)
      )

  return(library_list)
}
