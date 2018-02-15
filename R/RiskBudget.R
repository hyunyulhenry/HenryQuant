#' Risk Budget Portfolio
#'
#' This function solves for risk budge portfolio weights
#' @param covmat Covariance matrix
#' @param target Target of Risk Budget. The sum of budget should be 1
#' @importFrom nloptr slsqp
#' @examples
#' \dontrun{
#'   ret = asset_data
#'   R = ret[,c(1,5,9)]
#'   covmat = cov(R)
#'   target = c(0.4, 0.3, 0.3)
#'   weight = RiskBudget(covmat, target)
#'   }
#' @export
RiskBudget = function(covmat, target) {

  target = as.numeric(target)
  if (sum(target) != 1) {
    warning("Sum of Risk Budget is not 1, Check it again")
  }

  if ((ncol(covmat)) != length(target)) {
    warning("Length of Covariance Matrix and Target is not same")
  }

  #--- Risk Parity objective ---#
  RiskBudget_objective = function(x) {

    variance = t(x) %*% covmat %*% x
    sigma = sqrt(variance)
    MRC = (covmat %*% x) / as.numeric(sigma)

    rc = x * MRC
    rc = as.numeric(rc/sum(rc))

    sum_risk_diff = sum( (rc - target)^2 )

    return(sum_risk_diff)

  }

  #--- Inequality Objective ---#
  hin.objective = function(x) {
    return(x)
  }

  #--- Equality Objective ---#
  heq.objective = function(x) {
    return( sum(x) - 1 )
  }

  #--- Set Initial Seed (Equal Weight ---#
  x0.equal = rep(1/ncol(covmat), ncol(covmat))

  #--- Run Optimization ---#
  result = slsqp( x0 = x0.equal, fn = RiskBudget_objective,
                  hin = hin.objective,
                  heq = heq.objective,
                  control = list(xtol_rel = 1e-20, maxeval = 5000))

  return(round(result$par,4))

}
