#' Plot slingshot lineages on UMAP
#'
#' @param pt_lin The pseudotime column to plot
#' @param curves The slingshot curves to plot
#' @param add_lineage Add the lineage paths to the plot
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#'
#' pseudotime_df <- scGrabbag::extractSlinghot(sds)
#' df <- cbind(df, pseudotime_df)
#' pt_lineages <- grep("PT", names(df), value = T)
#' n_lineages <- length(pt_lineages)
#'
#' # Use purrr to generate plots for each PT column
#' plot_list <- purrr::map(pt_lineages, ~plotLineage(.x, curves, add_lineage = TRUE))
#' cowplot::plot_grid(plotlist = plot_list, align = 'v')
#' }
#'
#' @export
#'


plotLineage <- function(pt_lin, curves, add_lineage=FALSE){
  # Convert string to symbol for tidy evaluation
  lineage_enquo <- rlang::sym(pt_lin)
  # ordered_curves <- curves %>% arrange(Order)

  # Base plot
  p_pseudo <- ggplot(df, aes(x = UMAP_1, y = UMAP_2)) +
    geom_point(aes(fill = !!lineage_enquo), col = "black", shape = 21, alpha = 0.75) +
    scale_fill_viridis_c(option = "magma", direction = 1) +
    xlab("UMAP 1") +
    ylab("UMAP 2") +
    labs(fill = "pseudotime") +
    scGrabbag::personal_theme(legend_pos = "right", legend_keytype= "fillbar")

  # TODO: Resolve the issue curves issue. Potentially issue with dataframes being different size or rownames
  # Add lineage paths if required
  if (add_lineage == TRUE && exists("curves")) {

    p_pseudo <- p_pseudo +
      geom_path(data = curves, aes(group = Lineage, color = Lineage, linetype=Lineage),
                arrow = arrow(type = "open", length = unit(0.25, "inches")),
                linewidth = 3) +
      guides(colour = guide_legend(override.aes = list(linewidth = 2))) +
      khroma::scale_color_okabeito(reverse = F) +
      scale_linetype_manual(values = c("solid", "longdash"))
  }

  return(p_pseudo)
}
