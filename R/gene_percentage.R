#' Get percentage of gene expression greater than zero in dataset
#'
#' @param seurat_obj A Seurat object
#' @param gene A gene of interest
#'
#' @return A printed message containing gene expression percentage
#'
#'
#'


gene_percentage <- function(seurat_obj, gene) {
  # evaluate total population of gene+ cells in subclusters

  # Get total number of cells in the seurat dataset
  total_cells <- seurat_obj %>%
    ncol()

  # Calculate total number cells in which contain expression greater than zero in subclusters
  hits <- 0 # hits serves as our counter start
  gene_cells <-
    ifelse(seurat_obj[gene]@assays$RNA@counts > 0, hits + 1, hits + 0) %>% sum() # counter

  # Print message on the console with percent of gene found in subclusters
  message(
    'Percent of cells with \'',
    gene,
    '\' expression > 0 in subclusters : ',
    round(gene_cells / total_cells * 100, 2),
    "%"
  )

}
