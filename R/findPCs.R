#' Determine the number of PCs to use for downstream analysis
#'
#' This function was adapted from https://hbctraining.github.io/scRNA-seq/lessons/elbow_plot_metric.html
#'
#' @param seurat_object Seurat object
#' @param threshold_pct Threshold for percent of variation associated with each PC
#' @param threshold_cum_pct Threshold for cumulative percent of variation associated with each PC
#' @param threshold_diff_pct Threshold for difference between variation of PC and subsequent PC
#' @param elbow_plot Logical value to determine if an elbow plot should be generated
#'
#' @return Number of PCs to use for downstream analysis
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' findPCs(seurat_object,
#'         threshold_pct = 5,
#'         threshold_cum_pct = 90,
#'         threshold_diff_pct = 0.1,
#'         elbow_plot=TRUE)
#'



findPCs <- function(seurat_object,
                    threshold_pct = 5,
                    threshold_cum_pct = 90,
                    threshold_diff_pct = 0.1,
                    elbow_plot=FALSE){

  print("Thresholds for determining PCs")
  print(paste0("threshold_pct:", threshold_pct))
  print(paste0("threshold_cum_pct:", threshold_cum_pct))
  print(paste0("threshold_diff_pct:", threshold_diff_pct))

  # Calculate the percent of variation associated with each PC
  var_pct <- seurat_object[["pca"]]@stdev / sum(seurat_object[["pca"]]@stdev) * 100

  # Calculate cumulative percents for each PC
  cum_pct <- cumsum(var_pct)

  # Find the point where the cumulative percent is greater than 90% and the percent of variation is less than 5%
  cum_thresh_90pct <- which(cum_pct > threshold_cum_pct & var_pct < threshold_pct)[1]

  print(paste0("Threshold where cumulative percent is 90% or greater:", cum_thresh_90pct, " PCs \n"))

  # Determine the difference between variation of PC and subsequent PC
  small_pct_var <- sort(which(abs(diff(var_pct)) > threshold_diff_pct), decreasing = T)[1] + 1

  print(paste0("Threshold cutoff where PC variation is less than 0.1%:", small_pct_var, " PCs \n"))

  # Minimum of the two calculation
  pcs <- min(cum_thresh_90pct, small_pct_var)

  print(paste0("Minimum of the two thresholds:", pcs, " PCs \n"))

  # Elbow plot

  if(elbow_plot==TRUE){
    # Create a dataframe with values
    plot_df <- data.frame(var_pct = var_pct,
                          cum_pct = cum_pct,
                          rank = 1:length(var_pct))

    # Elbow plot to visualize
    eplot <-ggplot(plot_df, aes(cum_pct,
                                var_pct,
                                label = rank,
                                color = pcs > rank - 1)) +
      geom_text(size = 4,
                fontface="bold") +
      xlab("Cumulative Percent") +
      ylab("Percent of Variation for each PC") +
      geom_vline(xintercept = 90,
                 color = "black",
                 linetype = "dashed",
                 alpha = 0.5) +
      annotate("text",
               x = 90,
               y = max(var_pct),
               label = paste0("90% threshold - PC: ", cum_thresh_90pct),
               vjust = -1,
               hjust = 1,
               angle = 90,
               color = "black") +
      geom_vline(xintercept = plot_df$cum_pct[pcs],
                 color = "black",
                 linetype = "dotdash",
                 alpha = 0.75) +
      annotate("text",
               x = plot_df$cum_pct[pcs],
               y = max(var_pct),
               label = paste0("Selected PCs: ", pcs),
               vjust = -1,
               hjust = 1,
               angle = 90,
               color = "black") +
      scale_color_manual(name = "",
                         values = c("darkorange", "dodgerblue")) +
      theme_bw(base_size = 16) +
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = 'right',
            axis.text = element_text(color = 'black'),
            legend.text = element_text(color = 'black'))
  }

  print(eplot)
  return(pcs)

}
