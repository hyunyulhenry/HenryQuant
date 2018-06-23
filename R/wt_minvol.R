#' Minimum Volatility Portfolio
#'
#' This function solves for minimum volatility portfolio weights.
#' The lower limit and upper limit can be set differently for each asset.
#' @param covmat Covariance matrix
#' @param lb Lower weight boundary. Default is Null
#' @param ub Upper weight boundary. Default is Null
#' @param cut_w If the calculated weight is lower than the corresponding weight,
#' the weight is made zero. The default value is 1bp.
#' @importFrom quadprog solve.QP
#' @examples
#' \dontrun{
#'   ret = asset_data
#'   covmat = cov(ret)
#'   weight = wt_minvol(covmat, lb = rep(0, ncol(covmat)), ub = rep(1, ncol(covmat)))
#'   }
#' @export
wt_minvol = function(covmat, lb = NULL, ub = NULL, cut_w = 0.001) {

  if (is.null(lb)) {
    lb = rep(0, ncol(covmat))
  }

  if (is.null(ub)) {
    ub = rep(1, ncol(covmat))
  }

  n1 = length(lb)
  n2 = length(lb)

  Amat_mv = cbind(rep(1, n1), diag(n1), -diag(n1))
  bvec_mv = c(1, lb, -ub)
  w_mv = solve.QP(covmat,c(rep(0,n1)),Amat_mv,bvec_mv,1)$solution

  w_mv[ w_mv < cut_w ] = 0
  w_mv = w_mv / sum(w_mv)

  if(is.null(dimnames(covmat))){
    names(w_mv) = paste("Asset", 1:ncol(covmat), sep = "")
  } else {
    names(w_mv) = colnames(covmat)
  }
  return(w_mv)
}
