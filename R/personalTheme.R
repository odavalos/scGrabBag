#' My personal ggplot2 theme
#'
#' This function sets up a custom ggplot2 theme for styling plots consistently.
#' It provides options to customize various theme elements, including the size
#' of axis text, the size of the plot title, the position of the legend, and more.
#' This is currently a work in progress.
#'
#' @param theme_type Character string specifying the base theme to use. Options
#'   are "bw" (default) and "classic".
#' @param theme_bs Numeric value specifying the base font size for the theme.
#' @param ax_title Numeric value specifying the relative size of axis titles.
#' @param ax_text Numeric value specifying the relative size of axis text.
#' @param plt_title Numeric value specifying the relative size of plot titles.
#' @param x_angle Numeric value specifying the angle of x-axis text.
#' @param x_hjust Numeric value specifying the horizontal justification of x-axis text.
#' @param x_vjust Numeric value specifying the vertical justification of x-axis text.
#' @param striptext Character string specifying the strip text to customize. Options
#'   are "x", "y", or "right".
#' @param legend_pos Character string specifying the position of the legend. Options
#'   are "left", "right", "top", "bottom", or "none".
#' @param legend_keytype Character string specifying the type of legend key. Options
#'   are "point" (default) and "fillbar".
#' @param legend_title Numeric value specifying the relative size of legend titles.
#' @param legend_text Numeric value specifying the relative size of legend text.
#' @param legend_keysize Numeric value specifying the size of the legend key for "fillbar".
#' @param guide_color Numeric value specifying the size of the color guide.
#' @param guide_fill Numeric value specifying the size of the fill guide.
#' @param nopanels Logical value specifying whether to remove grid panels from the plot.
#' @return A list containing the custom theme and guide settings for ggplot2.
#'
#'
#' @examples
#' \dontrun{
#' # Create a custom theme with black and white color scheme
#' my_theme <- personalTheme(theme_type = "bw", theme_bs = 16, ax_title = 1.25,
#'                            ax_text = 1, plt_title = 2, x_angle = 0, x_hjust = 0.5,
#'                            x_vjust = 1, striptext = "x", legend_pos = "right",
#'                            legend_keytype = "point", legend_title = 1.25,
#'                            legend_text = 1.25, legend_keysize = 0.75,
#'                            guide_color = 4, guide_fill = 4)
#' }
#' @export


personalTheme <- function(theme_type = "bw",
                          theme_bs = 16,
                          ax_title = 1.25,
                          ax_text = 1,
                          plt_title = 2,
                          x_angle = 0,
                          x_hjust = 0.5,
                          x_vjust = 1,
                          striptext = NULL,
                          legend_pos = "right",
                          legend_keytype = "point",
                          legend_title = 1.25,
                          legend_text = 1.25,
                          legend_keysize = 0.75,
                          guide_color = 4,
                          guide_fill = 4,
                          nopanels = TRUE) {
  # Validate inputs
  # if (!theme_type %in% c("bw", "classic")) {
  #   stop("Invalid theme type. Choose 'bw' or 'classic'.")
  # }

  if (!legend_pos %in% c("left", "right", "top", "bottom", "none")) {
    stop("Invalid legend position. Choose 'left', 'right', 'top', 'bottom', or 'none'.")
  }

  # Base theme setup
  basetheme <- theme(
    axis.title = element_text(size = rel(ax_title)),
    axis.text = element_text(size = rel(ax_text), color = "black"),
    axis.text.x = element_text(angle = x_angle, hjust = x_hjust, color = "black", vjust = x_vjust),
    plot.title = element_text(size = rel(plt_title), hjust = 0.5),
    legend.text = element_text(size = rel(legend_text)),
    legend.title = element_text(size = rel(legend_title)),
    legend.position = legend_pos,
    rect = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent")
  )


  # Conditionally add legend key size for 'fillbar'
  if (legend_keytype == "fillbar") {
    basetheme <- basetheme + theme(legend.key.size = unit(legend_keysize, 'cm'))
  }

  # Conditionally remove panels from plot
  if (nopanels == TRUE){
    basetheme <- basetheme + theme(panel.grid.minor = element_blank(),
                                   panel.grid.major = element_blank())
  }

  # Dynamically add striptext settings if provided
  if (!is.null(striptext)) {
    striptext_elements <- switch(striptext,
                                 "x" = basetheme <- basetheme + theme(strip.text.x = element_text(size = rel(1.5), hjust = 0.5)),
                                 "y" = basetheme <- basetheme + theme(strip.text.y = element_text(size = rel(1.5), hjust = 0.5)),
                                 "right" = basetheme <- basetheme + theme(axis.title.y.right = element_text(face = "bold", size = rel(1.5), margin = margin(r = 7))),
                                 stop("Invalid striptext. Choose 'x', 'y', or 'right'.")
    )
  }

  # Theme object initialization based on type
  theme_obj <- switch(theme_type,
                      "bw" = theme_bw(base_size = theme_bs),
                      "classic" = theme_classic(base_size = theme_bs)
  )

  # Combine base theme with user-selected theme
  combined_theme <- theme_obj + basetheme

  # Set up guides based on legend type
  guide_settings <- if (legend_keytype == "point") {
    guides(
      color = guide_legend(override.aes = list(size = guide_color)),
      fill = guide_legend(override.aes = list(size = guide_fill))
    )
  } else {
    guides() # Default guide settings if not 'point'
  }

  return(list(combined_theme, guide_settings))
}
