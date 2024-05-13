#' Relative Abundance Dotplot
#'
#' @param srt_obj Seurat object
#' @param celltypes Character string of cell type column in metadata
#' @param group2 Character string of grouping column in metadata
#' @param celltype_pal Vector of colors for cell types
#' @param use_facet Logical for whether to use facet wrapping
#' @param plot_type Character string for plot type, either "dot" or "bar",
#' default is "dot"
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#'   abundanceDotplot(seuratdata, celltypes = "celltypes", group2 = "orig.ident",
#'   celltype_pal = celltype_cols, use_facet = FALSE)
#' }
#' @importFrom dplyr group_by summarize mutate
#' @importFrom rlang sym



abundanceDotplot <- function(srt_obj, celltypes, group2, celltype_pal, use_facet = FALSE, plot_type="dot") {
  # Convert to symbols for group_by and summarize
  celltypes_sym <- rlang::sym(celltypes)
  group2_sym <- rlang::sym(group2)

  df <- srt_obj@meta.data %>%
    group_by(!!celltypes_sym, !!group2_sym) %>%
    summarize(count = n()) %>%
    group_by(!!group2_sym) %>%
    mutate(proportion = count / sum(count))

  ra_plot <- switch(plot_type,
                    "dot" = ggplot(df, aes(x = !!group2_sym,
                                           y = proportion,
                                           color = !!celltypes_sym,
                                           fill = !!celltypes_sym,
                                           group = !!celltypes_sym)) +
                      geom_line(linewidth = 2, alpha = 0.75) +
                      geom_point(color = "black", size = 6, shape = 21) +
                      xlab("") +
                      ylab("Relative Abundance") +
                      ggtitle("") +
                      scale_color_manual(values = celltype_pal) +
                      scale_fill_manual(values = celltype_pal) +
                      personalTheme(nopanels = FALSE),
                    "bar" = ggplot(df, aes(x = !!group2_sym,
                                           y = proportion,
                                           color = !!celltypes_sym,
                                           fill = !!celltypes_sym,
                                           group = !!celltypes_sym)) +
                      geom_bar(stat = "identity",
                               width = 0.8,
                               color = "black") +
                      xlab("") +
                      ylab("Relative Abundance") +
                      ggtitle("") +
                      scale_color_manual(values = celltype_pal) +
                      scale_fill_manual(values = celltype_pal) +
                      personalTheme(nopanels = FALSE))


  # Conditionally add facet wrapping
  if (use_facet) {
    facet_formula <- rlang::expr(~ !!celltypes_sym)
    ra_plot <- ra_plot + facet_wrap(facet_formula, scales = "free_y")
  }

  return(ra_plot)
}
