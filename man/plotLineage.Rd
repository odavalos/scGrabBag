% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotLineage.R
\name{plotLineage}
\alias{plotLineage}
\title{Plot slingshot lineages on UMAP}
\usage{
plotLineage(pt_lin, curves, add_lineage = FALSE)
}
\arguments{
\item{pt_lin}{The pseudotime column to plot}

\item{curves}{The slingshot curves to plot}

\item{add_lineage}{Add the lineage paths to the plot}
}
\value{
A ggplot object
}
\description{
Plot slingshot lineages on UMAP
}
\examples{
\dontrun{

pseudotime_df <- scGrabbag::extractSlinghot(sds)
df <- cbind(df, pseudotime_df)
pt_lineages <- grep("PT", names(df), value = T)
n_lineages <- length(pt_lineages)

# Use purrr to generate plots for each PT column
plot_list <- purrr::map(pt_lineages, ~plotLineage(.x, curves, add_lineage = TRUE))
cowplot::plot_grid(plotlist = plot_list, align = 'v')
}

}
