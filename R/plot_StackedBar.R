#' Plot Stacked Barchart
#'
#' This function plot Stacked Barchart
#'
#' @param df Time Series data
#' @importFrom ggplot2 geom_area guides guide_legend
#'
#' @examples
#' \dontrun{
#'   plot_StackedBar(df)
#'   }
#' @export
plot_StackedBar = function(df) {

  Date = key = value = NULL

  df = df %>%
    data.frame() %>%
    rownames_to_column(var = 'Date') %>%
    mutate(Date = as.Date(Date)) %>%
    gather(key, value, -Date) %>%
    mutate(key = factor(key, levels = unique(key)))

  ggplot(df, aes(x = Date, y = value)) +
    geom_area(aes(color = key, fill = key), position = 'stack') +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw() +
    scale_x_date(date_breaks="years", date_labels="%Y",
                 expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(plot.title = element_text(hjust = 0.5,
                                    size = 12),
          legend.position = 'bottom',
          legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          panel.grid.minor.x = element_blank()) +
    guides(color = guide_legend(byrow = TRUE))

}
