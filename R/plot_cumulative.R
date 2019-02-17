#' Plot Cumulative Return
#'
#' This function plot Cumulative return
#'
#' @param R Return Data
#' @param ylog TRUE/FALSE set the y-axis to logarithmic scale
#' @importFrom xts apply.yearly
#' @importFrom dplyr mutate
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather
#' @importFrom lubridate year
#' @importFrom ggplot2 geom_line scale_x_date
#'
#' @examples
#'   R = asset_data
#'   plot_cumulative(R[,1])
#' @export
#'
plot_cumulative = function(R, ylog = FALSE) {

  Date = key = value = NULL

  R = as.xts(R) %>%
    na.fill(0)

  if (ylog == FALSE) {
    R.cum = cumprod(1 + R) - 1
  }

  if (ylog == TRUE) {
    R = log(1 + R)
    R.cum = cumsum(R)
  }

  R.cum = R.cum %>%
    data.frame() %>%
    rownames_to_column(var = 'Date') %>%
    mutate(Date = as.Date(Date)) %>%
    gather(key, value, -Date) %>%
    mutate(key = factor(key, levels = unique(key)))

  ggplot(R.cum, aes(x = Date, group = key, color = key)) +
    geom_line(aes(y = value)) +
    ggtitle('Portfolio Cumulative Return') +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw() +
    scale_x_date(date_breaks="years", date_labels="%Y",
                 expand = c(0, 0)) +
    theme(plot.title = element_text(hjust = 0.5,
                                    size = 12),
          legend.position = 'bottom',
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          panel.grid.minor.x = element_blank()) +
    guides(color = guide_legend(byrow = TRUE))

}
