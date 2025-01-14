% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractSlingshot.R
\name{extractSlinghot}
\alias{extractSlinghot}
\title{Extract pseudotime from Slingshot object}
\usage{
extractSlinghot(sds)
}
\arguments{
\item{sds}{Slingshot object}
}
\value{
Dataframe containing pseudotime values
}
\description{
Extract pseudotime values from Slingshot object and return as a dataframe
}
\examples{
\dontrun{
sds <- slingshot::slingshot(sce, clusterCol = "cluster")
pseudotime_df <- extractPseudotime(sds)
head(pseudotime_df)
}
}
