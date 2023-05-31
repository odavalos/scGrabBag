#' Run SingleR, scType and ProjectTILs on a Seurat object
#'
#' @param seurat_obj Seurat object
#' @param cluster_id Cluster ID column name
#' @return Data frame with SingleR, scType and ProjectTILs predictions
#'
#' @export
#'
#' @examples
#'
#' # do not add quotation marks around cluster_id
#' \dontrun{
#' runImmunePredictions(seurat_obj, seurat_cluster)
#' }
#'


runImmunePredictions <- function(seurat_obj, cluster_id){

  # Load libraries
  library(HGNChelper)
  library(openxlsx)
  library(ProjecTILs)

  # TODO: change to use enquo or curly-curly operator {{}}

  # enquo cluster_id for use in dplyr
  # cluster_id_enquo <- enquo(cluster_id)

  # # check if cluster_id is a string (not allowed) due to curly-curly operator {{}}
  # if(is.character(cluster_id)){
  #   stop(crayon::yellow(cli::symbol$arrow_right, " Cluster must not be a string!!!\n"))
  # }

  # SingleR ===============================================================================================================

  cat(crayon::magenta(cli::symbol$record, " Running SingleR...\n"))

  cat(crayon::green("\t",cli::symbol$arrow_right, " Generating predictions for Monaco...\n"))

  # Monaco Immune Data Preds
  ref_monaco <- celldex::MonacoImmuneData() # use label.fine
  pred_monaco <- SingleR::SingleR(test = seurat_obj@assays$RNA@data,
                                  ref = ref_monaco,
                                  labels = ref_monaco$label.fine,
                                  clusters = seurat_obj@meta.data[,quo_name(enquo(cluster_id))])
  pred_mon <- data.frame(pred_monaco[,"labels", drop = FALSE])
  colnames(pred_mon) <- "SingleR_Monaco"

  cat(crayon::green("\t",cli::symbol$arrow_right, " Generating predictions for ImmGen...\n"))

  # ImmGen Data Preds
  ref_immgen <- celldex::ImmGenData() # use label.fine
  pred_ig <- SingleR::SingleR(test = seurat_obj@assays$RNA@data,
                              ref = ref_immgen,
                              labels = ref_immgen$label.fine,
                              clusters = seurat_obj@meta.data[,quo_name(enquo(cluster_id))])
  pred_immgen <- data.frame(pred_ig[,"labels", drop = FALSE])
  colnames(pred_immgen) <- "SingleR_ImmGen"


  # scType Preds ==========================================================================================================

  # following code is adapted from https://github.com/IanevskiAleksandr/sc-type

  cat(crayon::magenta(cli::symbol$record, " Running scType...\n"))

  # load gene set preparation function
  source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
  # load cell type annotation function
  source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")

  # DB file
  db_ = "https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/ScTypeDB_full.xlsx";
  tissue = "Immune system" # e.g. Immune system,Pancreas,Liver,Eye,Kidney,Brain,Lung,Adrenal,Heart,Intestine,Muscle,Placenta,Spleen,Stomach,Thymus

  # prepare gene sets
  gs_list = gene_sets_prepare(db_, tissue)


  # get cell-type by cell matrix
  es.max = sctype_score(scRNAseqData = seurat_obj[["RNA"]]@scale.data,
                        scaled = TRUE,
                        gs = gs_list$gs_positive,
                        gs2 = gs_list$gs_negative)


  # merge by cluster
  cL_resutls = do.call("rbind", lapply(unique(seurat_obj@meta.data[,quo_name(enquo(cluster_id))]), function(cl){
    es.max.cl = sort(rowSums(es.max[ ,rownames(seurat_obj@meta.data[seurat_obj@meta.data[,quo_name(enquo(cluster_id))]==cl, ])]), decreasing = !0)
    head(data.frame(cluster = cl, type = names(es.max.cl), scores = es.max.cl, ncells = sum(seurat_obj@meta.data[,quo_name(enquo(cluster_id))]==cl)), 10)
  }))
  sctype_scores = cL_resutls %>% group_by(cluster) %>% top_n(n = 1, wt = scores)

  # set low-confident (low ScType score) clusters to "unknown"
  sctype_scores$type[as.numeric(as.character(sctype_scores$scores)) < sctype_scores$ncells/4] = "Unknown"


  seurat_obj@meta.data$customclassif = ""
  for(j in unique(sctype_scores$cluster)){
    cl_type = sctype_scores[sctype_scores$cluster==j,];
    seurat_obj@meta.data$customclassif[seurat_obj@meta.data[,quo_name(enquo(cluster_id))] == j] = as.character(cl_type$type[1])
  }


  preds_sctype_scores <- as.data.frame(sctype_scores)
  rownames(preds_sctype_scores) <- preds_sctype_scores$cluster
  preds_sctype_scores <- preds_sctype_scores %>% dplyr::select(type)
  colnames(preds_sctype_scores) <- "pred_sctype_scores"

  sctype_scores_2 = cL_resutls %>% group_by(cluster) %>% top_n(n = 5, wt = scores)



  # ProjectTILs ===========================================================================================================
  # ProjectTILs - TIL Atlas Mouse

  cat(crayon::magenta(cli::symbol$record, " Running ProjectTILs...\n"))

  cat(crayon::green("\t",cli::symbol$arrow_right, " Generating predictions for Mouse TIL Atlas...\n"))

  ref <- load.reference.map("./ProjecTILs_Datasets/ref_TILAtlas_mouse_v1.rds")


  seurat_obj <- ProjecTILs.classifier(query = seurat_obj,
                                      ref = ref,
                                      filter.cells = FALSE)




  preds_projecTILs_tilatlas <- seurat_obj@meta.data %>%
    group_by({{cluster_id}}, functional.cluster) %>%
    summarise(n = n()) %>%
    top_n(n = 1, wt = n) %>%
    dplyr::rename(preds_projecTILs_tilatlas = functional.cluster) %>%
    dplyr::select(preds_projecTILs_tilatlas)


  gc()

  # ProjectTILs - LCMV Atlas Mouse

  cat(crayon::green("\t",cli::symbol$arrow_right, " Generating predictions for Mouse LCMV Atlas...\n"))

  ref <- load.reference.map("./ProjecTILs_Datasets/ref_LCMV_Atlas_mouse_v1.rds")

  seurat_obj <- ProjecTILs.classifier(query = seurat_obj,
                                      ref = ref,
                                      filter.cells = FALSE)



  preds_projecTILs_lcmv <- seurat_obj@meta.data %>%
    group_by({{cluster_id}}, functional.cluster) %>%
    summarise(n = n()) %>%
    top_n(n = 1, wt = n) %>%
    dplyr::rename(preds_projecTILs_lcmvatlas = functional.cluster) %>%
    dplyr::select(preds_projecTILs_lcmvatlas)

  preds_projecTILs <- left_join(preds_projecTILs_tilatlas, preds_projecTILs_lcmv, by = quo_name(enquo(cluster_id))) %>%
    column_to_rownames(var = quo_name(enquo(cluster_id)))

  gc()

  # Save all predictions into a dataframe
  predictions_df <- cbind(pred_immgen, pred_mon, preds_sctype_scores, preds_projecTILs) %>%
    rownames_to_column(var = "cluster")

  # Save predictions to a list
  predictions_list <- list(predictions_df, sctype_scores_2)
  names(predictions_list) <- c("predictions_df", "sctype_scores_2")

  cat(crayon::green(cli::symbol$check, " Done!\n"))

  return(predictions_list)

}
