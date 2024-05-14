#' This function creates a directory for the sample and subdirectories for plots, data, and results.
#'
#' @param sample_name Name of the sample
#' @return A directory for the sample and subdirectories for plots, data, and results
#'
#' @export
#'
#' @examples
#' \dontrun{
#' prepAnalysisDir('sample1')
#' }



prepAnalysisDir <- function(sample_name){
  # directories
  workingdir <- paste0(getwd(), '/',sample_name)
  plotdir <- 'plots'
  datadir <- 'data'
  resultsdir <- 'results'

  # check if sample directory exists
  message('\nChecking if sample directory exists...')
  ifelse(test = !dir.exists(file.path(workingdir)),
         yes = {dir.create(file.path(workingdir));print(paste0('--> Sample directory for ', sample_name, ' created'))},
         no = {FALSE;print(paste0('--> Sample directory for ', sample_name, ' exists'))})

  # check for plot directory
  message('\nChecking if plot directory exists...')
  ifelse(test = !dir.exists(file.path(workingdir, plotdir)),
         yes = {dir.create(file.path(workingdir, plotdir));print(paste0('--> Plot directory for ', sample_name, ' created'))},
         no = {FALSE;print(paste0('--> Plot directory for ', sample_name, ' exists'))})

  # check for data directory
  message('\nChecking if data directory exists...')
  ifelse(test = !dir.exists(file.path(workingdir, datadir)),
         yes = {dir.create(file.path(workingdir, datadir));print(paste0('--> Data directory for ', sample_name, ' created'))},
         no = {FALSE;print(paste0('--> Data directory for ', sample_name, ' exists'))})

  # check for results directory
  message('\nChecking if results directory exists...')
  ifelse(test = !dir.exists(file.path(workingdir, resultsdir)),
         yes = {dir.create(file.path(workingdir, resultsdir));print(paste0('--> Results directory for ', sample_name, ' created'))},
         no = {FALSE;print(paste0('--> Results directory for ', sample_name, ' exists'))})

  setwd(workingdir)
  print(paste0('Working directory set to ', workingdir))

  message('\nDone!')


}
