
#' To cite the ifnb dataset, please use:
#'
#'   Kang et al. Multiplexed droplet single-cell RNA-sequencing using natural genetic variation Nature Biotech (2017)
#'
#' A BibTeX entry for LaTeX users is
#'
#' @Article{,
#'   author = {Hun Min Kang and Meena Subramaniam and Sasha Targ and Michelle Nguyen and Lenka Maliskova and Elizabeth McCarthy and Eunice Wan and Simon Wong and Lauren Byrnes and Cristina M Lanata and Rachel E Gate and Sara Mostafavi  and Alexandar Marson and Noah Zaitlen and Lindsey A Criswell and Chun Jimmie Ye},
#'  title = {Multiplexed droplet single-cell RNA-sequencing using natural genetic variation},
#'  journal = {Nature Biotechnology},
#'  year = {2017},
#'  doi = {doi:10.1038/nbt.4042},
#'  url = {https://www.nature.com/articles/nbt.4042},
#' }

library(tidyverse)
library(Seurat)

ifnb <- UpdateSeuratObject(ifnb.SeuratData::ifnb)

ifnb <- subset(ifnb, subset = orig.ident == "IMMUNE_CTRL")
Idents(ifnb) <- ifnb$seurat_annotations
ifnb <- subset(ifnb, downsample =100)
ifnb <- runQuickSeurat(ifnb, nfeats = 2000, clusters_res = 1)
ifnb <- addCelltypeColorPal(ifnb, celltypes = "seurat_annotations")
# saveRDS(ifnb, file = "~/Downloads/ifnb_example.rds")

usethis::use_data(ifnb, overwrite = TRUE)
