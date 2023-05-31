#' This function will create a barplot
#' of the top differentially expressed genes per cluster
#'
#' @param dex_df A dataframe of differentially expressed genes
#' @param n_degs The number of differentially expressed genes to plot per cluster
#' @param presto Logical value to determine if the dataframe is from presto
#' @param fc_line The fold change line to plot
#'
#' @return A barplot of the top differentially expressed genes per cluster
#' @export
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' topDEGBarplot(DEWilcox)
#' }
#'


topDEGBarplot <- function(dex_df, n_degs = 25, presto=FALSE, fc_line=0.25){

  # Check if the dataframe is from presto
  if(presto==TRUE){

    # rename a few columns
    dex_df <- dex_df %>%
      rename(gene = feature,
             avg_log2FC = logFC,
             p_val_adj = padj,
             cluster = group)

  }

  # Get top differentially expressed genes per cluster
  topdegs <- dex_df %>%
    filter(p_val_adj < 0.05) %>%
    group_by(cluster) %>%
    top_n(n = n_degs, wt = avg_log2FC)



  # Fix barplot not sorting genes properly in descending order. `tidytext::reorder_within()`
  # tidytext::reorder_within() is a godsend
  # pair this with tidytext::scale_y_reordered()
  # scale_y_reordered() will clean the gene name
  # adapted from https://juliasilge.com/blog/reorder-within/


  # Create the plot using facet_grid
  bplot <-ggplot(topdegs,
                 aes(x = avg_log2FC,
                     y = tidytext::reorder_within(gene, avg_log2FC, cluster))) +
    geom_bar(stat = "identity", fill = "grey50") +
    tidytext::scale_y_reordered() +
    facet_wrap(. ~ cluster, scales = "free_y") +
    labs(title = "Gene expression by cluster", x = "Average log2FC", y = "") +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          strip.text = element_text(size = 10,
                                    face = "bold"),
          axis.text.y = element_text(size = 8),
          axis.text.x = element_text(size = 8,
                                     hjust = 0),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(size = 12,
                                    face = "bold",
                                    hjust = 0.5,
                                    vjust = 1),
          plot.margin = unit(c(1,1,1,1), "cm")) +
    geom_vline(xintercept = c(0, fc_line))

  return(bplot)
}
