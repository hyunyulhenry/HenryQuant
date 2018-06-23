#' Maximum Diversification Portfolio
#'
#' This function solves for maximum diversification portfolio weights.
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
#'   weight = wt_maxdiv(covmat, lb = rep(0, ncol(covmat)), ub = rep(1, ncol(covmat)))
#'   }
#' @export
wt_maxdiv = function(covmat, lb = NULL, ub = NULL, cut_w = 0.001) {


  if (is.null(lb)) {
    lb = rep(0, ncol(covmat))
  }

  if (is.null(ub)) {
    ub = rep(1, ncol(covmat))
  }

  n = length(lb)

  Alb = -lb %*% matrix(1, 1, n) + diag(n)
  Aub = ub %*% matrix(1, 1, n) - diag(n)

  Amat = t(rbind(sqrt(diag(covmat)), Alb, Aub))
  bvec = c(1,rep(0,2*n))
  w_mdp = solve.QP(covmat,c(rep(0,n)),Amat,bvec,1)

  w_mdp = w_mdp$solution
  w_mdp = w_mdp / sum(w_mdp)
  w_mdp[w_mdp < cut_w] = 0
  w_mdp = w_mdp / sum(w_mdp)

  if(is.null(dimnames(covmat))){
    names(w_mdp) = paste("Asset", 1:ncol(covmat), sep = "")
  } else {
    names(w_mdp) = colnames(covmat)
  }
  return(w_mdp)
}
