#' Extract pseudotime from Slingshot object
#'
#' Extract pseudotime values from Slingshot object and return as a dataframe
#  ; if multiple lineages are detected, the average pseudotime is also calculated
#'
#' @param sds Slingshot object
#' @return Dataframe containing pseudotime values
#' @export
#' @examples
#' \dontrun{
#' sds <- slingshot::slingshot(sce, clusterCol = "cluster")
#' pseudotime_df <- extractPseudotime(sds)
#' head(pseudotime_df)
#' }



extractSlinghot <- function(sds){
  # Check if multiple lineages are detected
  if(ncol(sds@assays@data$pseudotime) > 1){
    message("Multiple lineages detected. . .")
    pseudotime_df <- as.data.frame(sds@assays@data$pseudotime)
    num_lin <- seq(ncol(pseudotime_df))
    avgpseudotime <- slingshot::slingAvgPseudotime(sds)
    pseudotime_df$avg <- avgpseudotime
    colnames(pseudotime_df) <- c(c(sapply(num_lin, function(x) paste0("L",x,"_PT"))), "Avg_PT")
  } else{
    message("Single lineage detected. . .")
    pseudotime_df <- as.data.frame(sds@assays@data$pseudotime)
    num_lin <- seq(ncol(pseudotime_df))
    colnames(pseudotime_df) <- c("L1_PT")
  }
  return(pseudotime_df)
}
