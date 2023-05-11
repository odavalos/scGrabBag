#' Generate multiple plots to visualize gene expression
#'
#'
#' @param dataset A seurat data object
#' @param genelist A list of genes
#' @param density A logical indicating whether to plot kernel density estimates or not
#' @param pal_large A logical indicating whether to use a large palette 'Tableau 20' or not
#'
#' @return Multiple plots showing clustering, expression, and or kernel density estimates for a gene/genes
#' @export
#'
#' @examples
#'

visualizeExpression <- function(dataset, genelist, density=FALSE, pal_large=FALSE){

  ## Generate all plots ##

  # cluster umap plot
  p_umapclusters <- Seurat::DimPlot(dataset,
                                    reduction = "umap",
                                    label = TRUE,
                                    repel = TRUE,
                                    label.size = 8) +
    {if(pal_large==TRUE){
      ggthemes::scale_colour_tableau(palette = 'Tableau 20', direction = -1)
    } else if(pal_large==FALSE){
      ggplot2::scale_color_brewer(palette = 'Dark2', direction = 1)
    }} +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    Seurat::NoLegend()

  # gene density plot
  p_density <- Nebulosa::plot_density(dataset, genelist, pal = 'magma')

  # gene expression plot
  p_gex <- Seurat::FeaturePlot(dataset, features = genelist) +
    ggplot2::labs(color = 'Expression')

  if(density==TRUE){

    bottom_row <- cowplot::plot_grid(p_density, p_gex)
    cowplot::plot_grid(p_umapclusters, bottom_row, ncol = 1)

  } else if(density==FALSE){

    cowplot::plot_grid(p_umapclusters, p_gex)

  }
}
