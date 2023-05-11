#' Function to create volcanoplot from Seurat scRNAseq differential expression data
#'
#' @param dex_df Dataframe of differential expression data
#' @param fc_cutoff Fold change cutoff for plotting
#' @param presto_de Logical value to determine if the dataframe is from presto
#' @param title Title of the plot
#' @param subtitle Subtitle of the plot
#' @param caption Caption of the plot
#'
#' @return A volcanoplot of the differential expression data
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @import ggrepel
#'
#' @examples
#' scVolcanoPlot(DEWilcox)
#'



scVolcanoPlot <- function(dex_df, fc_cutoff=2, presto_de=FALSE,title=NULL, subtitle=NULL, caption=NULL){

  # check if the dataframe is from presto
  # prep the data for plotting
  # deal with p-values at zero
  if(presto_de==TRUE){
    dex_df <- dex_df %>%
      rename(gene = feature,
             avg_log2FC = logFC,
             p_val_adj = padj) %>%
      mutate(p_val_adj = if_else(condition = p_val_adj < 1.074200e-299,
                                 true = 5.00000e-299,
                                 false = p_val_adj))
  } else {
    dex_df <- dex_df %>%
      rownames_to_column(var = 'gene') %>%
      mutate(p_val_adj = if_else(condition = p_val_adj < 1.074200e-299,
                                 true = 5.00000e-299,
                                 false = p_val_adj))
  }

  # siginificance threshold for p-value
  sig_p <- 1.30103 # this equates to pvalue = 0.05

  # create the plot
  vplot <- ggplot(dex_df, aes(x = avg_log2FC, y = -log10(p_val_adj))) +
    geom_point(color = 'gray') +
    geom_point(data = subset(dex_df, avg_log2FC > fc_cutoff & -log10(p_val_adj) > 2),
               aes(x = avg_log2FC, y = -log10(p_val_adj)), color = 'black') +
    ggrepel::geom_text_repel(data = subset(dex_df, avg_log2FC > fc_cutoff & -log10(p_val_adj) > 2),
                             aes(x = avg_log2FC, y = -log10(p_val_adj), label = gene)) +
    geom_point(data = subset(dex_df, avg_log2FC < -fc_cutoff & -log10(p_val_adj) > 2),
               aes(x = avg_log2FC, y = -log10(p_val_adj)), color = 'black') +
    ggrepel::geom_text_repel(data = subset(dex_df, avg_log2FC < -fc_cutoff & -log10(p_val_adj) > 2),
                             aes(x = avg_log2FC, y = -log10(p_val_adj), label = gene)) +
    geom_hline(yintercept = sig_p, color = 'red', linetype = 'dashed') +
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    theme_bw(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text = element_text(color = 'black'))

  # return the plot
  return(vplot)

}
