#' Plot Drawdown
#'
#' This function plot Drawdown
#'
#' @param R Return Data
#' @importFrom zoo na.fill
#' @importFrom PerformanceAnalytics Drawdowns
#' @importFrom ggplot2 scale_y_continuous
#' @examples
#'   R = asset_data
#'   plot_drawdown(R[,1])
#' @export
#'
plot_drawdown = function(R) {

  Date = key = value = NULL

  R = as.xts(R) %>%
    na.fill(0)

  R.drawdown = Drawdowns(R) %>%
    data.frame() %>%
    rownames_to_column(var = 'Date') %>%
    mutate(Date = as.Date(Date)) %>%
    gather(key, value, -Date) %>%
    mutate(key = factor(key, levels = unique(key)))

  ggplot(R.drawdown, aes(x = Date, group = key, color = key)) +
    geom_line(aes(y = value)) +
    ggtitle('Portfolio Drawdown') +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw() +
    scale_x_date(date_breaks="years", date_labels="%Y",
                 expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0.02, 0, 0)) +
    theme(plot.title = element_text(hjust = 0.5,
                                    size = 12),
          legend.position = 'bottom',
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          panel.grid.minor.x = element_blank()) +
    guides(color = guide_legend(byrow = TRUE))

}
