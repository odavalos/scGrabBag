#' Calculate the percentage of cells expressing a gene with counts above the median
#'
#' @param seurat_obj Seurat object
#' @param gene Gene name
#' @return Percent of cells with gene expression > 0 and > median
#'
#' @export
#'
#' @examples
#' gene_percentage(seurat_obj, "Cd8b")
#'



gene_percentage <- function(seurat_obj, gene) {

  # check that gene is present in seurat object
  if (!(gene %in% rownames(seurat_obj))) {
    stop(paste("Gene", gene, "not found in Seurat object"))
  }

  # get total number of cells in the seurat dataset
  total_cells <- ncol(seurat_obj)

  # calculate total number of cells with gene expression > 0 and > median
  gene_counts <- as.data.frame(seurat_obj[gene]@assays$RNA@counts)
  hits <- gene_counts > 0 & gene_counts > median(gene_counts[gene_counts > 0])
  gene_cells <- sum(hits)

  # calculate percent of cells with gene expression > 0 and > median
  gene_percent <- round(gene_cells / total_cells * 100, 2)

  # print result to console
  cat("Percent of cells with '", gene, "' expression > 0 and > median: ", gene_percent, "%\n", sep="")
  return(gene_percent)
}
