#' Plot Line
#'
#' This function plot Line Graph
#'
#' @param df Data Frame
#' @param na 'last' to lastest data, '0 to fill with 0
#' @importFrom xts apply.yearly
#' @importFrom dplyr mutate
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather
#' @importFrom lubridate year
#' @importFrom ggplot2 geom_line scale_x_date
#' @export
plot_line = function(df, na = 'last') {

  Date = key = value = NULL

  if (na == 'last') { df = df %>% na.locf()}
  if (na == '0') { df = df %>% na.fill(0)}

  df = df %>%
    data.frame() %>%
    rownames_to_column(var = 'Date') %>%
    mutate(Date = as.Date(Date)) %>%
    gather(key, value, -Date) %>%
    mutate(key = factor(key, levels = unique(key)))

  ggplot(df, aes(x = Date, group = key, color = key)) +
    geom_line(aes(y = value)) +
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
