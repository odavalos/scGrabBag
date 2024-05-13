#' Run SoupX using Seurat clustering
#'
#' @param filtered_cellranger_counts_dir Path to the filtered cellranger counts directory
#' @param raw_cellranger_counts_h5 Path to the raw cellranger counts h5 file
#' @param output_sample_name Name of the output file
#' @param output_type Type of output file. Either "rds" or "mtx"
#' @return A matrix of corrected counts
#' @export
#' @examples
#' \dontrun{
#' RunSoupX(filtered_cellranger_counts_dir = "filtered_cellranger_counts_dir",
#'          raw_cellranger_counts_h5 = "raw_cellranger_counts_h5",
#'          output_sample_name = "output_sample_name",
#'          output_type = "rds")
#' }
#'
#' @importFrom Seurat CreateSeuratObject NormalizeData FindVariableFeatures ScaleData RunPCA FindNeighbors FindClusters RunUMAP
#' @importFrom SoupX SoupChannel setSoupProfile setClusters autoEstCont adjustCounts
#' @importFrom Matrix writeMM



RunSoupX <- function(filtered_cellranger_counts_dir, raw_cellranger_counts_h5, output_sample_name, output_dir = NULL, output_type = "rds") {

  # check if the required packages are installed
  if(!requireNamespace("Seurat", quietly = TRUE)) {
    stop("Seurat is required to run this function. Please install Seurat and try again.")
  }
  if(!requireNamespace("SoupX", quietly = TRUE)) {
    stop("SoupX is required to run this function. Please install SoupX and try again.")
  }
  if(!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Matrix is required to run this function. Please install Matrix and try again.")
  }

  # check if the required arguments are provided
  if(output_type != "rds" & output_type != "mtx") {
    stop("output_type must be either 'rds' or 'mtx'")
  }
  if(!dir.exists(filtered_cellranger_counts_dir)) {
    stop("filtered_cellranger_counts_dir does not exist")
  }
  if(!file.exists(raw_cellranger_counts_h5)) {
    stop("raw_cellranger_counts_h5 does not exist")
  }

  # check if the output file already exists
  if(file.exists(paste0(output_sample_name,"_soupx_corrected_matrix.rds"))) {
    stop("output_sample_name already exists")
  }
  if(file.exists(paste0(output_sample_name,"_soupx_corrected_matrix.mtx"))) {
    stop("output_sample_name already exists")
  }


  # --------------------------------------------------------------------------------------------- #

  # Loading the data and running the seurat for clustering

  message("Loading raw data...\n")
  # load the raw droplet matrix
  data_rawdrops <- Read10X_h5(filename = raw_cellranger_counts_h5)


  message("Loading filtered data...\n")
  # Load the data
  filt_data <- Read10X(data.dir = filtered_cellranger_counts_dir)

  # Create the seurat object
  filt_seurat <- CreateSeuratObject(counts = filt_data,
                                    project = "filtered",
                                    min.cells = 0,
                                    min.features = 0)

  rm(filt_data)

  message("Running Seurat Pipeline...\n")

  # Run a quick seurat analysis for clustering
  filt_seurat <- filt_seurat %>%
    NormalizeData() %>%
    FindVariableFeatures() %>%
    ScaleData() %>%
    RunPCA()

  # Find PCs for clustering
  Determine_PCs <- function(seurat_object,
                            threshold_pct = 5,
                            threshold_cum_pct = 90,
                            threshold_diff_pct = 0.1,
                            elbow_plot=FALSE){

    print("Thresholds for determining PCs")
    print(paste0("threshold_pct:", threshold_pct))
    print(paste0("threshold_cum_pct:", threshold_cum_pct))
    print(paste0("threshold_diff_pct:", threshold_diff_pct))

    # Calculate the percent of variation associated with each PC
    var_pct <- seurat_object[["pca"]]@stdev / sum(seurat_object[["pca"]]@stdev) * 100

    # Calculate cumulative percents for each PC
    cum_pct <- cumsum(var_pct)

    # Find the point where the cumulative percent is greater than 90% and the percent of variation is less than 5%
    cum_thresh_90pct <- which(cum_pct > threshold_cum_pct & var_pct < threshold_pct)[1]

    print(paste0("Threshold where cumulative percent is 90% or greater:", cum_thresh_90pct, " PCs \n"))


    # Determine the difference between variation of PC and subsequent PC
    # threshold_diff_pct <- 0.1
    # small_pct_var <- sort(which((var_pct[1:length(var_pct) - 1] - lead(var_pct, default = 0)[-length(var_pct)]) > threshold_diff_pct), decreasing = T)[1] + 1

    # Determine the difference between variation of PC and subsequent PC
    small_pct_var <- sort(which(abs(diff(var_pct)) > threshold_diff_pct), decreasing = T)[1] + 1

    print(paste0("Threshold cutoff where PC variation is less than 0.1%:", small_pct_var, " PCs \n"))

    # Minimum of the two calculation
    pcs <- min(cum_thresh_90pct, small_pct_var)

    print(paste0("Minimum of the two thresholds:", pcs, " PCs \n"))

    # Elbow plot

    if(elbow_plot==TRUE){
      # Create a dataframe with values
      plot_df <- data.frame(var_pct = var_pct,
                            cum_pct = cum_pct,
                            rank = 1:length(var_pct))

      # Elbow plot to visualize
      ggplot(plot_df, aes(cum_pct,
                          var_pct,
                          label = rank,
                          color = pcs > rank - 1)) +
        geom_text(size = 4,
                  fontface="bold") +
        xlab("Cumulative Percent") +
        ylab("Percent of Variation for each PC") +
        geom_vline(xintercept = 90,
                   color = "black",
                   linetype = "dashed",
                   alpha = 0.5) +
        annotate("text",
                 x = 90,
                 y = max(pct),
                 label = paste0("90% threshold - PC: ", cum_thresh_90pct),
                 vjust = -1,
                 hjust = 1,
                 angle = 90,
                 color = "black") +
        geom_vline(xintercept = plot_df$cum_pct[pcs],
                   color = "black",
                   linetype = "dotdash",
                   alpha = 0.75) +
        annotate("text",
                 x = plot_df$cum_pct[pcs],
                 y = max(pct),
                 label = paste0("Selected PCs: ", pcs),
                 vjust = -1,
                 hjust = 1,
                 angle = 90,
                 color = "black") +
        scale_color_manual(name = "",
                           values = c("darkorange", "dodgerblue")) +
        theme_bw(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = 'right',
              axis.text = element_text(color = 'black'),
              legend.text = element_text(color = 'black'))
    }

    return(pcs)

  }

  pcs <- Determine_PCs(filt_seurat)

  # PC's to use
  use_pcs <- 1:pcs

  filt_seurat <- filt_seurat %>%
    FindNeighbors(reduction="pca",
                  dims = use_pcs) %>%
    FindClusters(resolution = c(0.2, 0.4, 0.5, 1)) %>%
    RunUMAP(reduction = "pca",
            dims = use_pcs,
            umap.method = 'uwot')

  # Get the cluster information
  soupx_groups <- filt_seurat$RNA_snn_res.0.5

  # Get the filtered counts
  data_counts <- filt_seurat@assays$RNA@counts

  # --------------------------------------------------------------------------------------------- #

  message("Running SoupX...\n")

  # Generate SoupChannel Object for SoupX
  sc <- SoupChannel(data_rawdrops,
                    data_counts,
                    calcSoupProfile = FALSE)

  # Add extra meta data to the SoupChannel object
  soupProf <- data.frame(row.names = rownames(data_counts),
                         est = rowSums(data_counts)/sum(data_counts),
                         counts = rowSums(data_counts))
  sc <- setSoupProfile(sc, soupProf)
  # Set cluster information in SoupChannel
  sc <- setClusters(sc, soupx_groups)

  # Estimate contamination fraction
  sc <- autoEstCont(sc, doPlot=FALSE)
  # Infer corrected table of counts and rount to integer
  out <- adjustCounts(sc, roundToInt = TRUE)

  #TODO: Currently saving twice!!!
  # Save the output
  if(output_type == "rds" & !is.null(output_dir)) {
    saveRDS(out, file=paste0(output_dir, output_sample_name,"_soupx_corrected_matrix.rds"))
  } else if(output_type == "mtx" & !is.null(output_dir)) {
    Matrix::writeMM(out, file=paste0(output_dir, output_sample_name, "_soupx_corrected_matrix.mtx"))
  } else {
    stop("output_type must be either 'rds' or 'mtx'")
  }

  if(output_type == "rds") {
    saveRDS(out, file=paste0(output_sample_name,"_soupx_corrected_matrix.rds"))
  } else if(output_type == "mtx") {
    Matrix::writeMM(out, file=paste0(output_sample_name, "_soupx_corrected_matrix.mtx"))
  } else {
    stop("output_type must be either 'rds' or 'mtx'")
  }


  message("Output saved to ", paste0(output_sample_name,"_soupx_corrected_matrix.", output_type), "\n")

  message("Done!\n")

  return(out)

}
