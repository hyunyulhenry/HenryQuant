#' Risk Budget Portfolio
#'
#' This function solves for risk budge portfolio weights
#' @param covmat Covariance matrix
#' @param target Target of Risk Budget. The sum of budget should be 1
#' @param optctrl Object of class Rcpp_CTRL.
#' @param ... Ellipsis argument is passed down to nlminb().
#' @importFrom cccp ctrl rp getx
#' @examples
#' \dontrun{
#'   ret = asset_data
#'   R = ret[,c(1,5,9)]
#'   covmat = cov(R)
#'   target = c(0.4, 0.3, 0.3)
#'   weight = RiskBudget(covmat, target)
#'   }
#' @export
wt_RiskBudget = RiskBudget = function (covmat, target=NULL, optctrl = ctrl(), ...){

  if(is.null(target)){
    target = rep(1/nrow(covmat), nrow(covmat))
  }

  N = ncol(covmat)

  call = match.call()
  ## calling rp() from cccp
  opt = rp(x0 = target, P = covmat, mrc = target, optctrl = optctrl)
  w = drop(getx(opt))
  w = w / sum(w)

  if(is.null(dimnames(covmat))){
    names(w) = paste("Asset", 1:N, sep = "")
  } else {
    names(w) = colnames(covmat)
  }
  return(w)
}

