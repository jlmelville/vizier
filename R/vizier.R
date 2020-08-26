#' Embedding Plot
#'
#' Plots the embedded coordinates, with each point colored by a specified
#' color.
#'
#' The \code{x} argument can be used to provide a suitable vector of colors
#' from either a data frame or vector.
#'
#' If a data frame is provided, then a vector of colors will be looked for. If
#' it's present, it will be used as the \code{colors} argument directly.
#' Otherwise, a factor column will be looked for, and each level will be mapped
#' to a different color. Otherwise, one color will be used for each point. If
#' more than one column of a type is found in the data frame, the last one
#' encountered is used.
#'
#' If a vector is provided, a similar procedure to the data frame is used when
#' mapping from its content to a vector of colors. Additionally, a numeric
#' vector can be provided, which will be linearly mapped to a color scheme.
#'
#' The \code{color_scheme} parameter can be one of:
#' \itemize{
#' \item A palette function that takes an integer \code{n} and returns a vector
#'  of colors, e.g. \code{grDevices::rainbow}. For some other applicable
#'  functions, see the \code{Palettes} help page in the \code{grDevices}
#'  package (e.g. by running the \code{?rainbow} command).
#' \item A vector of colors making up a custom palette of your own
#'  devising, e.g. \code{c('red', 'green', 'blue')}. There must be at least two
#'  colors in the list.
#' \item The name of a color scheme provided by the
#'  \href{https://cran.r-project.org/package=paletteer}{paletteer} package, in
#'  the form \code{"package::palette"}. Some examples include
#'  \code{"dutchmasters::milkmaid"}, \code{"cartography::green.pal"},
#'  \code{"viridis::inferno"} and \code{"RColorBrewer::Dark2"}. If more colors
#'  are required than supported by the color scheme, interpolation will be used
#'  to create the required number of colors.
#' \item The name of a pre-defined R palette, if you are running R 4.0 or later.
#'  Some examples include \code{"Okabe-Ito"}, \code{"Tableau 10"} and 
#'  \code{"Alphabet"}. See \code{\link[grDevices]{palette.pals}} for the list of
#'  possible names. Note that is function is not available prior to R 4.0.
#'  For more details, see \url{https://developer.r-project.org/Blog/public/2019/11/21/a-new-palette-for-r/index.html}.
#' }
#' 
#' If you just want one color for all points, then you can pass a single color
#' to the `colors` argument, e.g. \code{colors = "blue"}.
#'
#' @param coords Matrix of embedded coordinates, with as many rows as
#'  observations, and 2 columns.
#' @param x Either a data frame or a column that can be used to derive a
#'  suitable vector of colors. Ignored if \code{colors} is provided.
#' @param colors Vector containing colors for each observation. If fewer colors
#'  than than observations are provided, then the colors are recycled.
#' @param color_scheme A color scheme. See 'Details'. Ignored if \code{colors}
#'  is specified.
#' @param alpha_scale Scale the opacity alpha of the colors, between 0 and 1.
#'   Useful for increasing the transparency of points, especially with large
#'   plots with lots of overlap.
#' @param num_colors Number of distinct colors to use in the palette, when
#'   \code{x} is a numeric vector, on the assumption that the palette is
#'   continuous (which it probably should be). Ignored if \code{x} is not a
#'   numeric vector. If set to \code{NULL}, it will be set to \code{length(x)}.
#' @param limits The range that the colors should map over when mapping from a
#'  numeric vector. If not specified, then the range of \code{x}. This is useful
#'  if there is some external absolute scale that should be used. Ignored if
#'  \code{x} is not a numeric vector.
#' @param top If not \code{NULL}, only the specified number of points will be
#'  displayed, corresponding to those with the highest values in \code{vec},
#'  after sorting by decreasing order.
#' @param cex Size of the points. Ignored if \code{text} is provided.
#' @param text Vector of label text to display instead of a point. If the labels
#'  are long or the data set is large, this is unlikely to be very legible, but
#'  is occasionally useful.
#' @param title Title for the plot.
#' @param sub Subtitle for the plot. Appears below the title.
#' @param equal_axes If \code{TRUE}, the X and Y axes are set to have the same
#'   extents.
#' @param pc_axes If \code{TRUE}, the \code{coords} are replaced by the first
#'   two (unscaled) principal components, which should have the effect of
#'   rotating the data (with a potential reflection) so the main variance aligns
#'   along the X-axis. Should not have any other scaling effect.
#' @param xlim Vector two numeric value to give the numeric extent of the
#'   X-axis. Ignored if \code{equal_axes = TRUE} or \code{pc_axes = TRUE}.
#' @param ylim Vector two numeric value to give the numeric extent of the
#'   Y-axis. Ignored if \code{equal_axes = TRUE} or \code{pc_axes = TRUE}.
#' @param NA_color Color to use for \code{NA} values, which can arise if using
#'   a factor column for \code{x} (or if any item in \code{colors} is 
#'   \code{NA}). By default, these points won't be displayed.
#' @param rev logical indicating whether the ordering of the colors should be 
#'   reversed.
#' @param verbose If \code{TRUE}, log messages to the console, mainly when
#'   searching for a suitable color column in a dataframe.
#' @export
#' @examples
#' # Embed with PCA
#' pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)
#' # Visualize the resulting embedding, colored by iris species, using the
#' # rainbow color scheme
#' embed_plot(pca_iris$x, iris$Species, color_scheme = rainbow,
#'            title = "iris PCA", sub = "rainbow colors")
#'
#' # topo.colors scheme
#' embed_plot(pca_iris$x, iris$Species, color_scheme = topo.colors)
#'
#' # Pass in data frame and it will use the last (in this case, only) factor
#' # column it finds
#' embed_plot(pca_iris$x, iris)
#'
#' # Use the "Dark2" RColorBrewer scheme
#' embed_plot(pca_iris$x, iris, color_scheme = "RColorBrewer::Dark2")
#'
#' # Can plot the category names instead of points, but looks bad if they're
#' # long (or the dataset is large)
#' embed_plot(pca_iris$x, iris$Species, cex = 0.5, text = iris$Species)
#'
#' # Visualize numeric value (petal length) as a color
#' embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues")
#'
#' # Just show the points with the 10 longest petals
#' embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues", top = 10)
#'
#' # Can force axes to be equal size to stop clusters being distorted in one
#' # direction
#' embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues",
#'            equal_axes = TRUE)
embed_plot <- function(coords, x = NULL, colors = NULL,
                       color_scheme = grDevices::rainbow,
                       num_colors = 15, alpha_scale = 1,
                       limits = NULL, top = NULL,
                       cex = 1, title = NULL, text = NULL, sub = NULL,
                       equal_axes = FALSE, pc_axes = FALSE, 
                       xlim = NULL, ylim = NULL, 
                       NA_color = NULL,
                       rev = FALSE,
                       verbose = FALSE) {
  if (methods::is(coords, "list") && !is.null(coords$coords)) {
    coords <- coords$coords
  }

  if (is.null(colors)) {
    if (!is.null(x)) {
      res <- color_helper(x, color_scheme = color_scheme,
                          num_colors = num_colors, limits = limits, top = top,
                          verbose = verbose)
      if (!is.null(res$colors)) {
        colors <- res$colors
      }
      else {
        colors <- res$palette[res$labels]
      }
    }
    else {
      colors <- make_palette(
        ncolors = nrow(coords),
        color_scheme = color_scheme,
        verbose = verbose
      )
    }
  }
  if (!is.null(NA_color)) {
    colors[is.na(colors)] <- NA_color
  }
  
  colors <- grDevices::adjustcolor(colors, alpha.f = alpha_scale)
  if (rev) {
    colors <- rev(colors)
  }
  
  if (pc_axes) {
    coords <- pc_rotate(coords)
  }

  if (equal_axes) {
    lims <- base::range(coords)
    xlim <- lims
    ylim <- lims
  }

  if (!is.null(text)) {
    graphics::plot(coords,
                   type = "n", xlim = xlim, ylim = ylim,
                   xlab = "X", ylab = "Y", main = title
    )
    graphics::text(coords, labels = text, cex = cex, col = colors)
  }
  else {
    graphics::plot(coords,
                   pch = 20, cex = cex, col = colors,
                   xlim = xlim, ylim = ylim, xlab = "X", ylab = "Y",
                   main = title
    )
  }
  if (!is.null(sub)) {
    graphics::mtext(sub)
  }
}

#' Embedding Plot Using the JavaScript Library Plotly
#'
#' Plots the embedded coordinates, with each point colored by a specified
#' color, using Plotly.
#'
#' This will open a web browser if you are using the R CLI. In RStudio, it
#' will put the plot in RStudio's Plots tab.
#'
#' The \code{x} argument can be used to provide a suitable vector of colors
#' from either a data frame or vector.
#'
#' If a data frame is provided, then a vector of colors will be looked for. If
#' it's present, it will be used as the \code{colors} argument directly.
#' Otherwise, a factor column will be looked for, and each level will be mapped
#' to a different color. Otherwise, one color will be used for each point. If
#' more than one column of a type is found in the data frame, the last one
#' encountered is used.
#'
#' If a vector is provided, a similar procedure to the data frame is used when
#' mapping from its content to a vector of colors. Additionally, a numeric vector
#' can be provided, which will be linearly mapped to a color scheme.
#'
#' The \code{color_scheme} parameter can be one of:
#' \itemize{
#' \item A palette function that takes an integer \code{n} and returns a vector
#'  of colors, e.g. \code{grDevices::rainbow}. For some other applicable
#'  functions, see the \code{Palettes} help page in the \code{grDevices}
#'  package (e.g. by running the \code{?rainbow} command).
#' \item A vector of colors making up a custom color scheme of your own
#'  devising, e.g. \code{c('red', 'green', 'blue')}. There must be at least two
#'  colors in the list.
#' \item The name of a color scheme provided by the
#'  \href{https://cran.r-project.org/package=paletteer}{paletteer} package, in
#'  the form \code{"package::palette"}. Some examples include
#'  \code{"dutchmasters::milkmaid"}, \code{"cartography::green.pal"},
#'  \code{"viridis::inferno"} and \code{"RColorBrewer::Dark2"}. If more colors
#'  are required than supported by the color scheme, interpolation will be used
#'  to create the required number of colors.
#' }
#'
#' @param coords Matrix of embedded coordinates, with as many rows as
#'   observations, and 2 columns.
#' @param x Either a data frame or a column that can be used to derive a
#'   suitable vector of colors. Ignored if \code{colors} is provided.
#' @param colors Vector containing colors for each coordinate.
#' @param color_scheme A color scheme. See 'Details'. Ignored if \code{colors}
#'   is specified.
#' @param num_colors Number of distinct colors to use in the palette, when
#'   \code{x} is a numeric vector, on the assumption that the palette is
#'   continuous (which it probably should be). Ignored if \code{x} is not a
#'   numeric vector. If set to \code{NULL}, it will be set to \code{length(x)}.
#' @param alpha_scale Scale the opacity alpha of the colors, between 0 and 1.
#'   Useful for increasing the transparency of points, especially with large
#'   plots with lots of overlap.
#' @param cex Size of the points. Ignored if \code{text} is provided.
#' @param text Vector of label text to display instead of a point. If the labels
#'   are long or the data set is large, this is unlikely to be very legible, but
#'   is occasionally useful.
#' @param tooltip Vector of tooltip text, to be displayed when a point is
#'   hovered over.
#' @param title Title for the plot.
#' @param show_legend If \code{TRUE}, display a legend. Ignored unless a
#'   suitable categorical value is provided as \code{x} (or one can be found).
#' @param equal_axes If \code{TRUE}, the X and Y axes are set to have the
#'   same extents.
#' @param pc_axes If \code{TRUE}, the \code{coords} are replaced by the
#'   first two (unscaled) principal components, which should have the effect of
#'   rotating the data (with a potential reflection) so the main variance aligns
#'   along the X-axis. Should not have any other scaling effect.
#' @param xlim Vector two numeric value to give the numeric extent of the
#'   X-axis. Ignored if \code{equal_axes = TRUE} or \code{pc_axes = TRUE}.
#' @param ylim Vector two numeric value to give the numeric extent of the
#'   Y-axis. Ignored if \code{equal_axes = TRUE} or \code{pc_axes = TRUE}.
#' @param rev logical indicating whether the ordering of the colors should be 
#'   reversed.
#' @param verbose If \code{TRUE}, log messages to the console, mainly when
#'   searching for a suitable color column in a dataframe.
#'
#' More information on plotly is available at its website,
#' \url{https://plot.ly}.
#' @export
#' @examples
#' \dontrun{
#' # Embed with PCA
#' pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)
#'
#' # Load the plotly package
#' library("plotly")
#'
#' # Visualize the resulting embedding, colored by iris species, using the
#' # rainbow color scheme
#' embed_plotly(pca_iris$x, iris$Species, color_scheme = rainbow,
#'              title = "iris PCA")
#'
#' # topo.colors scheme
#' embed_plotly(pca_iris$x, iris$Species, color_scheme = topo.colors)
#'
#' # Pass in data frame and it will use the last (in this case, only) factor
#' # column it finds
#' embed_plotly(pca_iris$x, iris)
#'
#' # Don't have to see a legend if the tooltips will do
#' embed_plotly(pca_iris$x, iris, show_legend = FALSE)
#'
#' # Custom tooltips
#' embed_plotly(pca_iris$x, iris, tooltip = paste("Species:", iris$Species))
#'
#' # Use the "Dark2" RColorBrewer scheme
#' embed_plotly(pca_iris$x, iris, color_scheme = "RColorBrewer::Dark2")
#'
#' # Can plot the category names instead of points, but looks bad if they're
#' # long (or the dataset is large)
#' embed_plot(pca_iris$x, iris$Species, cex = 0.5, text = iris$Species)
#'
#' # Visualize numeric value (petal length) as a color
#' embed_plotly(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues")
#'
#' # Can force axes to be equal size to stop cluster being distorted in one
#' # direction
#' embed_plotly(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues",
#'              equal_axes = TRUE)
#' }
embed_plotly <- function(coords, x = NULL, colors = NULL,
                         color_scheme = grDevices::rainbow,
                         num_colors = 15, alpha_scale = 1,
                         title = NULL, show_legend = TRUE,
                         cex = 1, text = NULL, tooltip = NULL,
                         equal_axes = FALSE, pc_axes = FALSE,
                         xlim = NULL, ylim = NULL, rev = FALSE, 
                         verbose = FALSE) {
  if (methods::is(coords, "list") && !is.null(coords$coords)) {
    coords <- coords$coords
  }

  if (!is.null(text)) {
    mode <- "text"
    labels <- text
    marker <- NULL
  }
  else {
    mode <- "markers"
    marker <- list(size = cex * 6)
    labels <- NULL
  }

  use_palette <- FALSE
  if (!is.null(colors) && is.null(labels)) {
    labels <- colors
  }

  if (is.null(colors)) {
    if (!is.null(x)) {
      if (methods::is(x, "numeric")) {
        labels <- x
        if (methods::is(color_scheme, "character")) {
          colors <- make_palette(ncolors = num_colors,
                                 color_scheme = color_scheme)
        }
        else {
          colors <- color_scheme(max(1, num_colors - 1))
        }
        use_palette <- TRUE
        mode <- "markers"
        marker <- list(size = cex * 6)
      }
      else {
        res <- color_helper(x,
                            color_scheme = color_scheme,
                            verbose = verbose)
        if (!is.null(res$labels)) {
          use_palette <- TRUE
          labels <- res$labels
          colors <- res$palette
        }
        else {
          # Just manual colors
          colors <- res$colors
        }
      }
    }
    else {
      # one color per point
      colors <- make_palette(
        ncolors = nrow(coords),
        color_scheme = color_scheme,
        verbose = verbose
      )
      show_legend <- FALSE
    }
  }
  colors <- grDevices::adjustcolor(colors, alpha.f = alpha_scale)
  if (rev) {
    colors <- rev(colors)
  }
  
  if (pc_axes) {
    coords <- pc_rotate(coords)
  }

  if (equal_axes) {
    lims <- base::range(coords)
    xlim <- lims
    ylim <- lims
  }
  
  if (!is.null(tooltip)) {
    text <- tooltip
  }
  else if (is.null(text)) {
    text <- labels
  }
  # prepend "<index>: " to tooltips to identify point in dataframe
  text <- paste0(as.character(seq_len(length(text))), ": ", text)

  if (use_palette) {
    p <- plotly::plot_ly(
      x = coords[, 1], y = coords[, 2],
      color = ~labels,
      colors = colors,
      type = "scatter", mode = mode,
      text = text,
      marker = marker
    )
  }
  else {
    show_legend <- FALSE
    marker <- append(marker, list(color = colors))
    p <- plotly::plot_ly(
      x = coords[, 1], y = coords[, 2],
      type = "scatter", mode = mode,
      text = text,
      marker = marker
    )
  }
  p <-
    plotly::layout(p,
                   title = title,
                   xaxis = list(
                     title = "X",
                     zeroline = FALSE, showline = TRUE, showgrid = FALSE,
                     range = xlim * 1.15
                   ),
                   yaxis = list(
                     title = "Y",
                     zeroline = FALSE, showline = TRUE, showgrid = FALSE,
                     range = ylim
                   ),
                   showlegend = show_legend
    )
  if (show_legend && methods::is(x, "numeric")) {
    p <- plotly::colorbar(p, title = "")
  }
  else {
    p <- plotly::hide_colorbar(p)
  }
  p
}

# Given a data frame or a vector, return a vector of colors appropriately
# mapped to the color scheme.
# If \code{x} is a vector, it can either be a vector of colors, a factor vector
# or factor-like character vector (in which case each level is mapped to a
# color), or a numeric vector (in which case the range is mapped linearly). If
# \code{x} is a data frame, then it is checked for a color column. If there
# isn't one, a factor column (or character column that can be treated like a
# factor) is looked for. If there's more than one suitable column, the last
# found column is used. Numeric columns aren't searched for in the data frame
# case.
color_helper <- function(x,
                         color_scheme = grDevices::rainbow,
                         num_colors = 15, limits = NULL, top = NULL,
                         verbose = FALSE) {
  if (methods::is(x, "data.frame")) {
    res <- color_helper_df(x, color_scheme = color_scheme,
                           verbose = verbose)
  }
  else {
    res <- color_helper_column(x, color_scheme = color_scheme,
                               num_colors = num_colors, limits = limits,
                               top = top, verbose = verbose)
  }
  res
}


# Try and find a meaningful vector of colors from a data frame.
# If the data frame contains at least one column of colors, use the last column
# of colors found.
# Otherwise, if the data frame contains at least one column of factors, map
# from the last factor column found to a list of colors.
# Otherwise, if the data frame contains at least one character column, and it
# can be treated like a factor (i.e. more than one level but as many levels as
# observations), use the last character column found as if it was a factor.
# Otherwise, color each point as if it was its own factor level
# @note R considers numbers to be acceptable colors because \code{col2rgb}
# interprets them as indexes into a palette. Columns of numbers are NOT treated
# as colors by color_helper. Stick with color names (e.g. "goldenrod") or
# rgb strings (e.g. "#140000" or "#140000FF" if including alpha values).
# If ret_labels is TRUE, return the column used for the mapping
color_helper_df <- function(df,
                            color_scheme = color_scheme,
                            verbose = FALSE) {
  colors <- NULL
  labels <- NULL
  # Is there a color column?
  color_name <- last_color_column_name(df)
  if (!is.null(color_name)) {
    if (verbose) {
      message("Found color column '", color_name, "'")
    }
    colors <- df[[color_name]]
    return(list(colors = colors))
  }

  # Is there a factor column?
  label_name <- last_factor_column_name(df)
  if (!is.null(label_name)) {
    if (verbose) {
      message("Found a factor '", label_name, "' for mapping to colors")
    }
    labels <- df[[label_name]]
    palette <- factor_to_palette(labels,
                                 color_scheme = color_scheme,
                                 verbose = verbose)
    return(list(labels = labels, palette = palette))
  }

  # Is there something factorish?
  label_name <- last_character_column_name(df)
  if (!is.null(label_name) && is_factorish(df[[label_name]])) {
    if (verbose) {
      message("Found a character column '", label_name,
              "' for mapping to colors")
    }
    labels <- df[[label_name]]
    colors <- factor_to_palette(as.factor(labels),
                                color_scheme = color_scheme,
                                verbose = verbose)
    return(list(labels = labels, palette = palette))
  }

  # use one color per point
  if (verbose) {
    message("Using one color per point")
  }
  colors <- make_palette(ncolors = nrow(df), color_scheme = color_scheme)
  list(colors = colors, labels = labels)
}

color_helper_column <- function(x,
                                color_scheme = color_scheme,
                                num_colors = 15, limits = NULL, top = NULL,
                                verbose = FALSE) {
  # Is this a color column - return as-is
  if (is_color_column(x)) {
    return(list(colors = x))
  }

  # Is it numeric - map to continuous palette
  if (is.numeric(x)) {
    colors <- numeric_to_colors(x,
                                color_scheme = color_scheme,
                                n = num_colors, limits = limits
    )
    if (!is.null(top)) {
      svec <- sort(x, decreasing = TRUE)
      colors[x < svec[top]] <- NA
    }
    return(list(colors = colors))
  }

  # Is it a factor - map to palette (which should be categorical)
  if (is.factor(x)) {
    palette <- factor_to_palette(x, color_scheme = color_scheme,
                                 verbose = verbose)
    return(list(labels = x, palette = palette))
  }

  # Probably a column of characters, can they be treated as a factor?
  if (is_factorish(x)) {
    palette <- factor_to_palette(as.factor(x), color_scheme = color_scheme,
                                 verbose = verbose)
    return(list(labels = x, palette = palette))
  }

  # Otherwise one color per point (doesn't really matter what the palette is!)
  list(colors = make_palette(ncolors = length(x), color_scheme = color_scheme))
}

# Map a vector of factor levels, x, to a vector of colors taken from either
# a color ramp function, color scheme name or existing palette
#
# # ColorBrewer palette name
# factor_to_colors(iris$Species, color_scheme = "RColorBrewer::Set3")
# color ramp function
# factor_to_colors(iris$Species, color_scheme = rainbow)
factor_to_colors <- function(x, color_scheme = grDevices::rainbow,
                             verbose = FALSE) {
  factor_to_palette(x, color_scheme, verbose = verbose)[x]
}

# Map a vector of factor levels, x, to a palette based on the specified
# color scheme
factor_to_palette <- function(x, color_scheme = grDevices::rainbow,
                              verbose = FALSE) {
  category_names <- levels(x)
  ncolors <- length(category_names)
  make_palette(ncolors = ncolors, color_scheme = color_scheme,
               verbose = verbose)
}

# Map Numbers to Colors
#
# Maps a numeric vector to an equivalent set of colors based on a color scheme
#
# For numeric scales, the following RColorBrewer schemes may be useful:
# Sequential palettes names:
#  Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples
#  RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
# Diverging palette names:
#  BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
#
# This function is based off a Stack Overflow answer by user "Dave X":
#  \url{http://stackoverflow.com/a/18749392}
#
# @param x Numeric vector.
# @param name Name of the ColorBrewer palette.
# @param n Number of unique colors to map values in \code{x} to.
# @param limits The range that the colors should map over. If not specified,
#  then the range of \code{x}. This is useful if there is some external
#  absolute scale that should be used.
# @seealso
# More information on ColorBrewer is available at its website,
# \url{http://www.colorbrewer2.org}.
# @examples
# \dontrun{
# # Plot Iris dataset sepal width vs length, colored by petal length, using
# # 20 colors ranging from Purple to Green (PRGn):
# plot(iris[, c("Sepal.Length", "Sepal.Width")], cex = 1.5, pch = 20,
#  col = numeric_to_colors(iris$Petal.Length, color_scheme = "RColorBrewer::PRGn", n = 20))
#
# # Use the rainbow color ramp function
# plot(iris[, c("Sepal.Length", "Sepal.Width")], cex = 1.5, pch = 20,
#  col = numeric_to_colors(iris$Petal.Length, color_scheme = rainbow, n = 20))
# }
numeric_to_colors <- function(x, color_scheme = "RColorBrewer::Blues", n = NULL,
                              limits = NULL) {
  if (is.null(n)) {
    n <- length(x)
  }
  if (is.null(limits)) {
    limits <- range(x)
  }
  pal <- make_palette(ncolors = n, color_scheme = color_scheme)
  pal[findInterval(x, seq(limits[1], limits[2], length.out = length(pal) + 1),
                   all.inside = TRUE
  )]
}

# Color Palette with Specified Number of Colors
#
# Returns a palette with the specified size, based on an existing palette,
# color scheme name or color ramp function.
make_palette <- function(ncolors, color_scheme = grDevices::rainbow,
                         verbose = FALSE) {
  if (methods::is(color_scheme, "function")) {
    palette <- color_scheme(ncolors)
  }
  else {
    palette <- make_palette_function(color_scheme, verbose = verbose)(ncolors)
  }
  palette
}

# Custom Palette Function
#
# This function returns a function that creates a palette of a specified size
# based on either an existing palette or a named color scheme, interpolating
# if necessary.
make_palette_function <- function(name, verbose = FALSE) {
  if (length(name) > 1) {
    # Actually this is already a palette
    f <- function(n) {
      if (n > length(name)) {
        if (verbose) {
          message("Interpolating palette for ", n, " colors")
        }
        palette <- grDevices::colorRampPalette(name)(n)
      }
      else {
        palette <- name
      }
      palette
    }
    return(f)
  }
  
  type <- NULL
  pal <- NULL
  split_res <- unlist(strsplit(name, "::"))
  if (length(split_res) < 2 || length(split_res) > 3) {
    # For >= R 4.0, can supply name of built-in palette, e.g. "Okabe-Ito"
    if (length(split_res) == 1 && is_r_palette(split_res)) {
      pal <- list(
        palette = split_res,
        length = length(grDevices::palette.colors(palette = split_res)),
        type = "r"
      )
      split_res <- c("R", split_res[1])
    }
    else {
      stop(
        "Bad palette name '", name, "'. ",
        "Should be in format: <package>::<palette>[::<d|c>]"
      )
    }
  }
  
  package_name <- split_res[1]
  palette_name <- split_res[2]
  if (length(split_res) == 3) {
    type <- tolower(split_res[3])
    if (!type %in% c("c", "d")) {
      stop("Discrete palette type must be one 'd' or 'c'")
    }
  }
  
  if (is.null(pal)) {
    pal_df <- paletteer_everything()
    pal <- pal_df[pal_df$package == package_name, ]
    if (nrow(pal) == 0) {
      stop("Unknown package '", package_name, "'")
    }
  
    pal <- pal[pal$palette == palette_name, ]
    if (nrow(pal) == 0) {
      stop(
        "Unknown palette '", palette_name,
        "' for package '", package_name, "'"
      )
    }
  }

  pal_fn <- switch(as.character(pal$type),
                   "r" = function(package_name, palette_name, n) {
                     grDevices::palette.colors(n = n, palette = palette_name)
                   },
                   "c" = function(package_name, palette_name, n) {
                     pack_and_pal <- paste0(package_name, "::", palette_name)
                     forceAndCall(2, paletteer::paletteer_c, pack_and_pal, n)
                   },
                   "d" = function(package_name, palette_name, n) {
                     pack_and_pal <- paste0(package_name, "::", palette_name)
                     forceAndCall(3, paletteer::paletteer_d,
                                  pack_and_pal, n = n, type = type)
                   },
                   "dynamic" = function(package_name, palette_name, n) {
                     pack_and_pal <- paste0(package_name, "::", palette_name)
                     forceAndCall(2, paletteer::paletteer_dynamic, 
                                  pack_and_pal, n)
                   }
  )
  max_colors <- pal$length
  function(n) {
    ncols <- n
    if (n > max_colors) {
      if (verbose) {
        message("Interpolating palette for ", n, " colors from ", max_colors)
      }
      ncols <- max_colors
    }
    palette <- forceAndCall(4, pal_fn, package_name, palette_name, ncols)
    if (n > max_colors) {
      palette <- grDevices::colorRampPalette(palette)(n)
    }
    palette
  }
}

# Looks at all the columns in a data frame, returning the name of the last
# column which is a factor or NULL if there are no factors present.
last_factor_column_name <- function(df) {
  factor_name <- NULL
  factor_names <- filter_column_names(df, is.factor)
  if (length(factor_names) > 0) {
    factor_name <- factor_names[length(factor_names)]
  }
  factor_name
}

# Looks at all the columns in a data frame, returning the name of the last
# column which contains colors or NULL if there are no colors present.
last_color_column_name <- function(df) {
  color_column_name <- NULL
  color_column_names <- filter_column_names(df, is_color_column)
  if (length(color_column_names) > 0) {
    color_column_name <- color_column_names[length(color_column_names)]
  }
  color_column_name
}

# Looks at all the columns in a data frame, returning the name of the last
# column which is a character or NULL if there are no character columns present.
last_character_column_name <- function(df) {
  char_name <- NULL
  char_names <- filter_column_names(df, is.character)
  if (length(char_names) > 0) {
    char_name <- char_names[length(char_names)]
  }
  char_name
}

# returns TRUE if vector x consists of colors
is_color_column <- function(x) {
  !is.numeric(x) && all(is_color(x))
}

# Applies pred to each column in df and returns the names of each column that
# returns TRUE.
filter_column_names <- function(df, pred) {
  names(df)[(vapply(df, pred, logical(1)))]
}

# Given a vector of character types x, returns a vector of the same length,
# where each element is a boolean indicating if the element in x is a valid
# color.
# @note Taken from
# \url{http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation}
# @note numeric values are always seen as being valid colors!
# @examples
# is_color(c(NA, "black", "blackk", "1", "#00", "#000000", 1000))
#  <NA>   black  blackk       1     #00 #000000    1000
#  TRUE    TRUE   FALSE    TRUE   FALSE    TRUE    TRUE
is_color <- function(x) {
  vapply(x, function(X) {
    tryCatch(is.matrix(grDevices::col2rgb(X)),
             error = function(e) FALSE
    )
  }, logical(1))
}

# Given a vector of character, could it be usefully treated as a factor? To be
# factor-like, should have more than one level but not as many as one level per
# observation.
is_factorish <- function(x) {
  if (!methods::is(x, "character")) {
    return(FALSE)
  }
  x_factor <- as.factor(x)
  nlevels <- length(levels(x_factor))
  nlevels > 1 && nlevels < length(x_factor)
}

is_r_palette <- function(name) {
  exists("palette.pals", where="package:grDevices") && 
    name %in% grDevices::palette.pals()
}

# Does PCA and returns the first two components from the X. When X is a 2D
# matrix, this effectively rotates (and potentially reflects) the point set
# so the data aligns along the PCs.
pc_rotate <- function(X) {
  X <- scale(X, center = TRUE, scale = FALSE)
  s <- svd(X, nu = 2, nv = 0)
  s$u %*% diag(c(s$d[1:2]))
}

# Stuff all paletteer name data frames into one uniform frame
# containing package, palette and length.
# continuous palettes are considered to have an infinite length
paletteer_everything <- function() {
  all_packages <- c(
    paletteer::palettes_c_names$package,
    paletteer::palettes_d_names$package,
    paletteer::palettes_dynamic_names$package
  )
  all_palettes <- c(
    paletteer::palettes_c_names$palette,
    paletteer::palettes_d_names$palette,
    paletteer::palettes_dynamic_names$palette
  )
  all_lengths <- c(
    rep(Inf, nrow(paletteer::palettes_c_names)),
    paletteer::palettes_d_names$length,
    paletteer::palettes_dynamic_names$length
  )
  all_types <- c(
    rep("c", nrow(paletteer::palettes_c_names)),
    rep("d", nrow(paletteer::palettes_d_names)),
    rep("dynamic", nrow(paletteer::palettes_dynamic_names))
  )

  data.frame(
    package = all_packages, palette = all_palettes,
    length = all_lengths, type = all_types
  )
}
