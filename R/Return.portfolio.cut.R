#' Based on Return.portfolio function, it adds loss cut function.
#'
#' If specific assets exceed the Loss Cut limit before rebalancing,
#' sell all of its assets that exceed the cut point.
#' Frequency of return should be shorter than frequency of weights.#'
#'
#' @param R An xts, vector, matrix, data frame, timeSeries or zoo object of
#' asset returns
#' @param weights A time series or single-row matrix/vector containing asset
#' weights, as decimal percentages, treated as beginning of period weights.  See Details below.
#' @param loss_cut Loss Cut point. Default point is -10 to avoid loss cut.
#' @param rebalance_on Default "none"; alternatively "daily" "weekly" "monthly" "annual"  to specify calendar-period rebalancing supported by \code{endpoints}.
#' @param value The beginning of period total portfolio value. This is used for calculating position value.
#' @param verbose If verbose is TRUE, return a list of intermediary calculations.
#' See Details below.
#' @param \dots any other passthru parameters. Not currently used.
#' @return returns a time series of returns weighted by the \code{weights}, applied loss cut function.
#' @importFrom xts last periodicity first endpoints
#' @importFrom zoo coredata
#' @importFrom PerformanceAnalytics checkData
#' @examples
#' \dontrun{
#' data(edhec)
#' Return.portfolio.cut(edhec["1997",1:5], rebalance_on="quarters", loss_cut = -0.10)
#' }
#' @export
Return.portfolio.cut <- function(R,
                                 weights=NULL,
                                 loss_cut=-10,
                                 rebalance_on=c(NA, 'years', 'quarters', 'months', 'weeks', 'days'),
                                 value=1,
                                 verbose=FALSE,
                                 ...){

  R = checkData(R, method="xts")
  rebalance_on = rebalance_on[1]

  if(any(is.na(R))){
    warning("NA's detected: filling NA's with zeros")
    #R <- zerofill(R)
    R[is.na(R)] <- 0
  }

  # find the right unit to subtract from the first return date to create a start date
  freq = periodicity(R)
  switch(freq$scale,
         seconds = { stop("Use a returns series of daily frequency or higher.") },
         minute = { stop("Use a returns series of daily frequency or higher.") },
         hourly = { stop("Use a returns series of daily frequency or higher.") },
         daily = { time_unit = "day" },
         weekly = { time_unit = "week" },
         monthly = { time_unit= "month" },
         quarterly = { time_unit = "quarter" },
         yearly = { time_unit = "year"}
  )

  # calculates the end of the prior period
  start_date = seq(as.Date(index(R)[1]), length = 2, by = paste("-1", time_unit))[2]

  if(is.null(weights)){
    # generate equal weight vector for return columns
    weights = rep(1 / NCOL(R), NCOL(R))
  }
  if(is.vector(weights)) { # weights are a vector
    if(is.na(rebalance_on)) { # and endpoints are not specified
      # then use the weights only at the beginning of the returns series, without rebalancing
      weights = xts(matrix(weights, nrow=1), order.by=as.Date(start_date))
    } else { # and endpoints are specified
      #  generate a time series of the given weights at the endpoints
      weight_dates = c(as.Date(start_date), index(R[endpoints(R, on=rebalance_on)]))
      weights = xts(matrix(rep(weights, length(weight_dates)), ncol=NCOL(R), byrow=TRUE), order.by=as.Date(weight_dates))
    }
    colnames(weights) = colnames(R)
  } else { # check the beginning_weights object for errors
    # check that weights are given in a form that is probably a time series
    weights = checkData(weights, method="xts")
    # make sure that frequency(weights)<frequency(R) ?

    # make sure the number of assets in R matches the number of assets in weights
    # Should we also check the names of R and names of weights?
    if(NCOL(R) != NCOL(weights)){
      if(NCOL(R) > NCOL(weights)){
        R = R[, 1:NCOL(weights)]
        warning("number of assets in beginning_weights is less than number of columns in returns, so subsetting returns.")
      } else {
        stop("number of assets is greater than number of columns in returns object")
      }
    }
  } # we should have good weights objects at this point

  if(as.Date(last(index(R))) < (as.Date(index(weights[1,]))+1)){
    stop(paste('last date in series',as.Date(last(index(R))),'occurs before beginning of first rebalancing period',as.Date(first(index(weights)))+1))
  }

  # Subset the R object if the first rebalance date is after the first date
  # in the return series
  if(as.Date(index(weights[1,])) > as.Date(first(index(R)))) {
    R <- R[paste0(as.Date(index(weights[1,]))+1, "/")]
  }

  # bop = beginning of period
  # eop = end of period
  # Initialize objects
  bop_value = matrix(0, NROW(R), NCOL(R))
  colnames(bop_value) = colnames(R)
  eop_value = bop_value
  if(verbose){
    bop_weights = bop_value
    eop_weights = bop_value
    period_contrib = bop_value
  }

  ret = eop_value_total = bop_value_total = vector("numeric", NROW(R))

  # The end_value is the end of period total value from the prior period
  end_value <- value

  # initialize counter
  k = 1
  for(i in 1:NROW(weights)) {
    # identify rebalance from and to dates (weights[i,], weights[i+1]) and
    # subset the R(eturns) object
    from = as.Date(index(weights[i,]))+1
    if (i == nrow(weights)){
      to = as.Date(index(last(R))) # this is correct
    } else {
      to = as.Date(index(weights[(i+1),]))
    }
    returns = R[paste0(from, "::", to)]

    # Make Cumulative Return
    if (from > to) {
      ret_cum = cumprod(1+returns[,1])
    } else {
      ret_cum = cumprod(1+returns) - 1
    }

    # Only enter the loop if we have a valid returns object
    if(nrow(returns) >= 1){
      # inner loop counter
      jj = 1
      for(j in 1:nrow(returns)){
        # We need to know when we are at the start of this inner loop so we can
        # set the correct beginning of period value. We start a new inner loop
        # at each rebalance date.
        # Compute beginning of period values

        if(jj == 1){
          bop_value[k,] = end_value * weights[i,]
          roll_min = cummin(ret_cum) ###

        } else {
          bop_value[k,] = eop_value[k-1,]
        }
        bop_value_total[k] = sum(bop_value[k,])

        # Compute end of period values
        eop_value[k,] = (1 + coredata(returns[j,])) * bop_value[k,]
        eop_value_total[k] = sum(eop_value[k,])


        p = which( roll_min[j, ] <= loss_cut )
        if ( (length(p) != 0) & (j != nrow(returns)) ) {
          returns[j+1, p] = 0
        }

        if(verbose){
          # Compute bop and eop weights
          bop_weights[k,] = bop_value[k,] / bop_value_total[k]
          eop_weights[k,] = eop_value[k,] / eop_value_total[k]
          # Compute period contribution
          period_contrib[k,] = returns[j,] * bop_value[k,] / sum(bop_value[k,])
        }

        # Compute portfolio returns
        # Could also compute this by summing contribution, but this way we
        # don't have to compute contribution if verbose=FALSE
        ret[k] = eop_value_total[k] / end_value - 1

        # Update end_value
        end_value = eop_value_total[k]

        # increment the counters
        jj = jj + 1
        k = k + 1

      }
    }
  }
  R.idx = index(R)
  ret = xts(ret, R.idx)
  colnames(ret) = "portfolio.returns"

  if(verbose){
    out = list()
    out$returns = ret
    out$contribution = xts(period_contrib, R.idx)
    out$BOP.Weight = xts(bop_weights, R.idx)
    out$EOP.Weight = xts(eop_weights, R.idx)
    out$BOP.Value = xts(bop_value, R.idx)
    out$EOP.Value = xts(eop_value, R.idx)
  } else {
    out = ret
  }
  return(out)
}
