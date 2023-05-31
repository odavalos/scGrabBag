#' A function to run a quick Seurat analysis
#'
#' @param seurat_object A Seurat object
#' @param nfeats The number of features to use for HVG selection
#' @param clusters_res A vector of cluster resolutions to use
#'
#' @return A Seurat object
#' @export
#'
#' @examples
#' \dontrun{
#' runQuickSeurat(seurat_object = pbmc_small)
#' }
#'
#' @importFrom Seurat NormalizeData FindVariableFeatures ScaleData RunPCA FindNeighbors FindClusters RunUMAP
#'

runQuickSeurat <- function(seurat_object,
                           nfeats = 3000,
                           clusters_res = c(0.2, 0.4, 0.5, 1)) {

  # Normalize, select HVGs, scale, and run PCA
  seurat_object <- seurat_object %>%
    NormalizeData() %>%
    FindVariableFeatures(nfeatures = nfeats) %>%
    ScaleData() %>%
    RunPCA()

  # Find the number of PCs to use using the `findPCs` function
  pcs <- scGrabBag::findPCs(seurat_object)

  # PC's to use
  use_pcs <- 1:pcs

  # Run clustering and UMAP
  seurat_object <- seurat_object %>%
    FindNeighbors(reduction = "pca",
                  dims = use_pcs) %>%
    FindClusters(resolution = clusters_res) %>%
    RunUMAP(reduction = "pca",
            dims = use_pcs,
            umap.method = 'uwot')

  return(seurat_object)
}
