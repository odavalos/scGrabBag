#' This function creates a stacked barplot from a dataframe
#'
#' @param df dataframe
#' @param group_label column name of the dataframe to be used as grouping variable
#' @param xlabel x-axis label
#' @return a stacked barplot
#'
#' @export
#'
#' @import ggplot2
#' @import dplyr
#'
#'
#' @examples
#' stackedBarplot(iris, group_label = Species)


stackedBarplot <- function(df, group_label, xlabel = ""){

  # Find the number of unique groups
  n_unique <- length(unique(df[quo_name(enquo(group_label))][,1]))

  # Create the plot
  sb_plot <- df %>%
    group_by({{group_label}}) %>%
    summarise(n = n()) %>%
    ggplot(aes(x = reorder({{group_label}}, -n),
               y = n,
               fill = {{group_label}})) +
    geom_bar(stat="identity",
             width = 0.8,
             color = "black") +
    geom_text(aes(label = paste0(n, "\n")),
              position = position_stack(vjust = 1),
              size = 4) +
    labs(x = xlabel,
         y = "Cells",
         fill = "") +
    {if(n_unique > 12){
      ggthemes::scale_fill_tableau(palette = "Tableau 20", direction = 1)
    } else {
      scale_fill_brewer(palette = "Paired")
    }} +
    scale_fill_brewer(palette = "Paired") +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1),
          axis.text = element_text(color = 'black'),
          plot.title = element_text(hjust = 0.5),
          legend.position = "right",
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 12))

  return(sb_plot)

}
