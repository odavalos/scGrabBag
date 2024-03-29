% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runQuickSeurat.R
\name{runQuickSeurat}
\alias{runQuickSeurat}
\title{A function to run a quick Seurat analysis}
\usage{
runQuickSeurat(
  seurat_object,
  nfeats = 3000,
  clusters_res = c(0.2, 0.4, 0.5, 1)
)
}
\arguments{
\item{seurat_object}{A Seurat object}

\item{nfeats}{The number of features to use for HVG selection}

\item{clusters_res}{A vector of cluster resolutions to use}
}
\value{
A Seurat object
}
\description{
A function to run a quick Seurat analysis
}
\examples{
\dontrun{
runQuickSeurat(seurat_object = pbmc_small)
}

}
