#' Number of unique clusters generated per resolution
#'
#' @param seurat_data A Seurat object
#' @param min_res A logical indicating whether to identify resolution with minimum number of clusters
#'
#' @return Returns either the size of unique clusters generated or specific resolution with smallest unique cluster size
#'
#' @example
#' getresolutionsizes(seurat_data, min_res=FALSE)

getresolutionsizes <- function(seurat_data, min_res = FALSE) {
  if (min_res == TRUE) {
    # find number of unique clusters for each cluster resolution
    res <-
      grep("res", colnames(seurat_data@meta.data), value = TRUE)
    cluster_nums <- res %>% purrr::map_chr( ~ length(unique(seurat_data@meta.data[, .x])))

    # make a dataframe for unique clusters
    df <- data.frame(res = res, clusters = cluster_nums)

    # get index for smallest resolution
    idx <- which.min(df$clusters)

    # pull the unique resolution
    min_r <- df[idx, 'res']

    return(min_r)

  } else if (min_res == FALSE)

    # grep all columns that contain 'res' in metadata and determin the size of clusters generated
    resolutions <-
      grep("res", colnames(seurat_data@meta.data), value = TRUE) %>%
      purrr::map_chr( ~ paste(.x, "--> clusters generated:", length(unique(
        seurat_data@meta.data[, .x]
      ))))
  return(resolutions)
}
