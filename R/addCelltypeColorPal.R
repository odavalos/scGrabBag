#' Add celltype color palette to Seurat object
#'
#' This function adds a color palette to the metadata of a Seurat object based on the celltype column.
#' The function uses color palettes from khroma, ggthemes, and Rcolorbrewer.
#' https://github.com/tesselle/khroma
#' Frerebeau, N. (2024). khroma: Colour Schemes for Scientific Data Visualization. https://doi.org/10.5281/zenodo.1472077
#' https://github.com/jrnold/ggthemes
#' https://cran.r-project.org/web/packages/RColorBrewer/index.html
#' https://colorbrewer2.org/
#'
#' @param srt_obj Seurat object
#' @param celltypes character string of the column name in the metadata of the Seurat object that contains the celltype information
#' @return srt_obj Seurat object with celltype_cols a vector of colors
#' @export
#' @examples
#' \dontrun{
#' srt_obj <- addCelltypeColorPal(srt_obj, "celltypes")
#' }
#' @importFrom dplyr group_by summarise arrange pull
#' @importFrom rlang sym
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggthemes tableau_color_pal
#' @importFrom khroma color
#' @importFrom grDevices colorRampPalette




addCelltypeColorPal <- function(srt_obj, celltypes){
  # This function uses color pals from
  # khroma, ggthemes, Rcolorbrewer
  # https://github.com/tesselle/khroma
  # Frerebeau, N. (2024). khroma: Colour Schemes for Scientific Data Visualization. https://doi.org/10.5281/zenodo.1472077

  # Convert to symbols for group_by and sort by size
  celltypes_sym <- rlang::sym(celltypes)

  sortedbysize <- srt_obj@meta.data %>%
    group_by(!!celltypes_sym) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    pull(!!celltypes_sym)


  # generate color palette
  pal_list <- list(dark2 = colorRampPalette(RColorBrewer::brewer.pal(8,"Dark2")),
                   paired = colorRampPalette(RColorBrewer::brewer.pal(12,"Paired")),
                   # tab20 = ggthemes::tableau_color_pal(palette = "Tableau 20"),
                   bright = khroma::color("bright"),
                   vibrant = khroma::color("vibrant"),
                   light = khroma::color("light"),
                   muted = khroma::color("muted"),
                   okabe = khroma::color("okabeito"))

  # generate color palette
  if(length(sortedbysize) > 8){
    tab20 <- ggthemes::tableau_color_pal(palette = "Tableau 20")
    celltype_cols <- tab20(length(sortedbysize))
  } else {

    pal <- sample(names(pal_list), 1)
    celltype_cols <- do.call(pal_list[[pal]], args = list(length(sortedbysize)))

  }
  names(celltype_cols) <- sortedbysize


  # colorpalette to metadata
  srt_obj@meta.data$celltype_cols <- celltype_cols[match(srt_obj@meta.data[,celltypes], names(celltype_cols))]

  # convert the celltypes column into a factor sorted by size to keep colors consistent
  srt_obj@meta.data[,celltypes] <- factor(srt_obj@meta.data[,celltypes],
                                          levels = sortedbysize)

  return(srt_obj)

}
