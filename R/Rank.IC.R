#' Calculate Cumulative Return Series
#'
#' @param Rank Rank Data for Factor
#' @param R Return Data
#' @importFrom xts as.xts
#' @export
Rank.IC = function(Rank, R) {

  Rank = checkData(Rank, method="xts")
  R = checkData(R, method="xts")

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

  if(NCOL(Rank) != NCOL(R)){
    stop("number of assets in rank is less than number of columns in returns")
  }

  if(as.Date(last(index(R))) < (as.Date(index(Rank[1,]))+1)){
    stop(paste('last date in series',as.Date(last(index(R))),'occurs before beginning of first rebalancing period',as.Date(first(index(Rank)))+1))
  }

  # Subset the R object if the first rebalance date is after the first date
  # in the return series
  if(as.Date(index(Rank[1,])) > as.Date(first(index(R)))) {
    R <- R[paste0(as.Date(index(Rank[1,]))+1, "/")]
  }

  R_monthly = apply.monthly(R, Return.cumulative)

  rank_corr = list()

  for(i in 1:(NROW(Rank)-1)) {

    from = as.Date(index(Rank[i,]))
    if (i == nrow(Rank)){
      to = as.Date(index(last(R)))
    } else {
      to = as.Date(index(Rank[(i+1),]))
    }

    sub_rank = Rank[i, ]
    sub_ret = R_monthly[paste0(from+1, "::", to)]

    sub_list = list()
    for(j in 1:nrow(sub_ret)) {
      temp_cor = cor(as.numeric(sub_rank), as.numeric(-sub_ret[j, ]), use="complete.obs", method="spearman")
      sub_list[[j]] = xts(temp_cor, order.by = index(sub_ret[j, ]))
    }

    sub_list = do.call(rbind, sub_list)
    rank_corr[[i]] = sub_list
  }

  rank_corr = do.call(rbind, rank_corr)
  colnames(rank_corr) = "Rank Correlation"

  return(rank_corr)
}
