#' List of my favorite color palettes
#'
#' Use \code{grabbag_pal} to pull palette of interest
#'
#' Color palettes were collected from:
#' https://marketing.ucmerced.edu/resources/brand-guidelines/colors
#' https://thenode.biologists.com/data-visualization-with-flying-colors/research/
#' https://doi.org/10.1038/nmeth.1618
#' https://personal.sron.nl/~pault/
#'
#'@export
grabbag_pal_list <- list(
  ucm_pal = c('#002856', '#DAA900', '#0091B3','#F18A00','#64A43A', '#005487', '#FFBF3C', '#99D9D9'),
  # ucm_pal2 = c('#002856', '#DAA900'),
  # ucm_pal3 = c('#002856', '#DAA900', '#0091B3'),
  # ucm_pal4 = c('#002856', '#DAA900', '#0091B3','#F18A00'),
  ito_pal = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
  tol_bright = c('#EE6677', '#228833', '#4477AA', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB'),
  tol_light = c('#BBCC33', '#AAAA00', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#DDDDDD'),
  tol_muted = c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499', '#DDDDDD')
)




#' A custom palette generator
#'
#' Code generates custom length color palettes from a collection of color blind
#' friendly palettes
#'
#' @param pal_name name of color palette. Options include:
#' \code{ucm_pal}, \code{ito_pal}, \code{tol_bright}, \code{tol_light},
#' \code{tol_muted}
#' @param n number of colors to output
#'
#' @return a vector containing desired colors
#' @export
#'
#' @examples
#' grabbag_pal('ucm_pal', 2)
#' grabbag_pal('ito_pal', 8)
#' grabbag_pal('tol_muted', 10)
grabbag_pal <- function(pal_name, n){
  # make sure types are correct
  pal_name <- as.character(pal_name)
  # n <- as.integer(n)

  # get the palette
  pal <- grabbag_pal_list[[pal_name]]
  if(is.null(pal)) {
    stop("palette '" + pal_name + "' not found")
  }
  # get the colors from the palette
  if(missing(n)){
    n <- length(pal)
  }
  # return the colors
  pal_out <- pal[1:n]
  return(pal_out)
}
