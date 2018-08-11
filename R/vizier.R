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
#' mapping from its content to a vector of colors. Additionally, a numeric vector
#' can be provided, which will be linearly mapped to a color scheme.
#'
#' The \code{color_scheme} parameter can be one of either a color ramp function,
#' accepting an integer n as an argument and returning n colors, or the name of
#' a ColorBrewer color scheme. Probably should be one of the "Qualitative" set.
#'
#' For some applicable color ramp functions, see the \code{Palettes} help page
#' in the \code{grDevices} package (e.g. by running the \code{?rainbow} command).
#'
#' @param coords Matrix of embedded coordinates, with as many rows as
#'  observations, and 2 columns.
#' @param x Either a data frame or a column that can be used to derive a
#'  suitable vector of colors. Ignored if \code{colors} is provided.
#' @param colors Vector containing colors for each coordinate.
#' @param color_scheme Either a color ramp function, or the name of a
#'  ColorBrewer scheme. See 'Details'.
#' @param num_colors Number of unique colors to map to from \code{x}, if
#'  \code{x} is a numeric vector. Otherwise ignored.
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
#' @param verbose If \code{TRUE}, log messages to the console, mainly when
#'   searching for a suitable color column in a dataframe.
#' @note Use of this function with ColorBrewer qualitative palette names
#' requires that the \code{RColorBrewer} package be installed.
#'
#' More information on ColorBrewer is available at its website,
#'  \url{http://www.colorbrewer2.org}.
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
#' # Use the "Dark2" ColorBrewer scheme (needs RColorBrewer package installed)
#' library("RColorBrewer")
#' embed_plot(pca_iris$x, iris, color_scheme = "Dark2")
#'
#' # Can plot the category names instead of points, but looks bad if they're
#' # long (or the dataset is large)
#' embed_plot(pca_iris$x, iris$Species, cex = 0.5, text = iris$Species)
#'
#' # Visualize numeric value (petal length) as a color (the "Blues" scheme also
#' # needs RColorBrewer)
#' embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "Blues")
#'
#' # Just show the points with the 10 longest petals
#' embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "Blues", top = 10)
#'
#' # Can force axes to be equal size to stop cluster being distorted in one
#' # direction
#' embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "Blues",
#'            equal_axes = TRUE)
embed_plot <- function(coords, x = NULL, colors = NULL,
                       color_scheme = grDevices::rainbow,
                       num_colors = 15, limits = NULL, top = NULL,
                       cex = 1, title = NULL, text = NULL, sub = NULL,
                       equal_axes = FALSE, pc_axes = FALSE, verbose = FALSE) {
  if (methods::is(coords, "list") && !is.null(coords$coords)) {
    coords <- coords$coords
  }

  if (is.null(colors)) {
    if (!is.null(x)) {
      colors <- color_helper(x,
        color_scheme = color_scheme,
        num_colors = num_colors, limits = limits,
        top = top, verbose = verbose
      )$colors
    }
    else {
      colors <- make_palette(
        ncolors = nrow(coords),
        color_scheme = color_scheme
      )
    }
  }

  if (pc_axes) {
    coords <- pc_rotate(coords)
  }

  lims <- NULL
  if (equal_axes) {
    lims <- range(coords)
  }

  if (!is.null(text)) {
    graphics::plot(coords,
      type = "n", xlim = lims, ylim = lims,
      xlab = "X", ylab = "Y", main = title
    )
    graphics::text(coords, labels = text, cex = cex, col = colors)
  }
  else {
    graphics::plot(coords,
      pch = 20, cex = cex, col = colors,
      xlim = lims, ylim = lims, xlab = "X", ylab = "Y",
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
#' The \code{color_scheme} parameter can be one of either a color ramp function,
#' accepting an integer n as an argument and returning n colors, or the name of
#' a ColorBrewer color scheme. Probably should be one of the "Qualitative" set.
#'
#' For some applicable color ramp functions, see the \code{Palettes} help page
#' in the \code{grDevices} package (e.g. by running the \code{?rainbow} command).
#'
#' @param coords Matrix of embedded coordinates, with as many rows as
#'  observations, and 2 columns.
#' @param x Either a data frame or a column that can be used to derive a
#'  suitable vector of colors. Ignored if \code{colors} is provided.
#' @param colors Vector containing colors for each coordinate.
#' @param color_scheme Either a color ramp function, or the name of a
#'  ColorBrewer scheme. See 'Details'.
#' @param cex Size of the points. Ignored if \code{text} is provided.
#' @param text Vector of label text to display instead of a point. If the labels
#'  are long or the data set is large, this is unlikely to be very legible, but
#'  is occasionally useful.
#' @param tooltip Vector of tooltip text, to be displayed when a point is
#'  hovered over.
#' @param title Title for the plot.
#' @param show_legend If \code{TRUE}, display a legend. Ignored unless a
#' suitable categorical value is provided as \code{x} (or one can be found).
#' @param equal_axes If \code{TRUE}, the X and Y axes are set to have the
#'  same extents.
#' @param pc_axes If \code{TRUE}, the \code{coords} are replaced by the
#' first two (unscaled) principal components, which should have the effect of
#' rotating the data (with a potential reflection) so the main variance aligns
#' along the X-axis. Should not have any other scaling effect.
#' @note Use of this function requires installing and loading the
#' \code{plotly} package, using version 4 or above.
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
#' # Use the "Dark2" ColorBrewer scheme (needs RColorBrewer package installed)
#' library("RColorBrewer")
#' embed_plotly(pca_iris$x, iris, color_scheme = "Dark2")
#'
#' # Can plot the category names instead of points, but looks bad if they're
#' # long (or the dataset is large)
#' embed_plot(pca_iris$x, iris$Species, cex = 0.5, text = iris$Species)
#'
#' # Visualize numeric value (petal length) as a color (the "Blues" scheme also
#' # needs RColorBrewer)
#' embed_plotly(pca_iris$x, iris$Petal.Length, color_scheme = "Blues")
#'
#' # Can force axes to be equal size to stop cluster being distorted in one
#' # direction
#' embed_plotly(pca_iris$x, iris$Petal.Length, color_scheme = "Blues",
#'              equal_axes = TRUE)
#' }
embed_plotly <- function(coords, x = NULL, colors = NULL,
                         color_scheme = grDevices::rainbow,
                         title = NULL, show_legend = TRUE,
                         cex = 1, text = NULL, tooltip = NULL,
                         equal_axes = FALSE, pc_axes = FALSE) {
  if (!requireNamespace("plotly", quietly = TRUE, warn.conflicts = FALSE)) {
    stop("embed_plotly function requires 'plotly' package")
  }

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

  if (!is.null(colors) && is.null(labels)) {
    labels <- colors
  }

  if (is.null(colors)) {
    if (!is.null(x)) {
      if (methods::is(x, "numeric")) {
        labels <- x
        if (methods::is(color_scheme, "character")) {
          colors <- color_scheme
        }
        else {
          colors <- color_scheme(length(x))
        }
        mode <- "markers"
        marker <- list(size = cex * 6)
      }
      else {
        res <- color_helper(x,
          color_scheme = color_scheme,
          ret_labels = TRUE
        )
        colors <- res$colors
        if (!is.null(res$labels)) {
          labels <- res$labels
        }
        else {
          labels <- colors
        }
      }
    }
    else {
      colors <- make_palette(
        ncolors = nrow(coords),
        color_scheme = color_scheme
      )
      labels <- colors
      show_legend <- FALSE
    }
  }

  if (pc_axes) {
    coords <- pc_rotate(coords)
  }

  lims <- NULL
  if (equal_axes) {
    lims <- range(coords)
  }

  if (!is.null(tooltip)) {
    text <- tooltip
  }
  else if (is.null(text)) {
    text <- labels
  }

  # prepend "<index>: " to tooltips to identify point in dataframe
  text <- paste0(as.character(seq_len(length(text))), ": ", text)

  p <- plotly::plot_ly(
    x = coords[, 1], y = coords[, 2],
    color = labels,
    colors = colors,
    type = "scatter", mode = mode,
    text = text,
    marker = marker
  )
  p <-
    plotly::layout(p,
      title = title,
      xaxis = list(
        title = "X",
        zeroline = FALSE, showline = TRUE, showgrid = FALSE,
        range = lims * 1.15
      ),
      yaxis = list(
        title = "Y",
        zeroline = FALSE, showline = TRUE, showgrid = FALSE,
        range = lims
      ),
      showlegend = show_legend
    )
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
                         ret_labels = FALSE,
                         verbose = FALSE) {
  if (methods::is(x, "data.frame")) {
    res <- color_helper_df(x,
      color_scheme = color_scheme,
      ret_labels = ret_labels,
      verbose = verbose
    )
    if (!ret_labels) {
      res <- list(colors = res, labels = NULL)
    }
  }
  else {
    if (methods::is(x, "factor")) {
      labels <- x
    }
    else {
      labels <- NULL
    }
    res <- list(
      colors = color_helper_column(x,
        color_scheme = color_scheme,
        num_colors = num_colors, limits = limits, top = top,
        verbose = verbose
      ),
      labels = labels
    )
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
color_helper_df <- function(df,
                            color_scheme = color_scheme,
                            ret_labels = FALSE,
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
  }

  if (is.null(colors)) {
    # Is there a factor column?
    label_name <- last_factor_column_name(df)
    if (!is.null(label_name)) {
      if (verbose) {
        message("Found a factor '", label_name, "' for mapping to colors")
      }
      labels <- df[[label_name]]
      colors <- factor_to_colors(labels, color_scheme = color_scheme)
    }
  }

  if (is.null(colors)) {
    label_name <- last_character_column_name(df)
    if (!is.null(label_name) && is_factorish(df[[label_name]])) {
      if (verbose) {
        message(
          "Found a character column '", label_name,
          "' for mapping to colors"
        )
      }
      labels <- df[[label_name]]
      colors <- factor_to_colors(as.factor(labels), color_scheme = color_scheme)
    }
  }

  if (is.null(colors)) {
    if (verbose) {
      message("Using one color per point")
    }
    # use one color per point
    colors <- make_palette(ncolors = nrow(df), color_scheme = color_scheme)
  }

  # Return a list with both results if we want labels, otherwise just colors
  if (ret_labels) {
    res <- list(colors = colors, labels = labels)
  }
  else {
    res <- colors
  }
  res
}

color_helper_column <- function(x,
                                color_scheme = color_scheme,
                                num_colors = 15, limits = NULL, top = NULL,
                                verbose = FALSE) {
  # Is this a color column - return as-is
  if (is_color_column(x)) {
    return(x)
  }

  # Is it numeric - map to palette (which should be sequential or diverging)
  if (is.numeric(x)) {
    colors <- numeric_to_colors(x,
      color_scheme = color_scheme,
      n = num_colors, limits = limits
    )
    if (!is.null(top)) {
      svec <- sort(x, decreasing = TRUE)
      colors[x < svec[top]] <- NA
    }
    return(colors)
  }

  # Is it a factor - map to palette (which should be categorical)
  if (is.factor(x)) {
    return(factor_to_colors(x, color_scheme = color_scheme))
  }

  # Probably a column of characters, can they be treated as a factor?
  if (is_factorish(x)) {
    return(factor_to_colors(as.factor(x), color_scheme = color_scheme))
  }

  # Otherwise one color per point (doesn't really matter what the palette is!)
  make_palette(ncolors = length(x), color_scheme = color_scheme)
}


# Map a vector of factor levels, x,  to a vector of colors taken from either
# an RColorBrewer palette name, or a color ramp function.
# @examples
# factor_to_colors(iris$Species, color_scheme = "Set3") # ColorBrewer palette
# factor_to_colors(iris$Species, color_scheme = rainbow) # color ramp function
factor_to_colors <- function(x, color_scheme = grDevices::rainbow) {
  category_names <- levels(x)
  ncolors <- length(category_names)
  color_palette <- make_palette(ncolors = ncolors, color_scheme = color_scheme)
  color_palette[x]
}


# Map Numbers to Colors
#
# Maps a numeric vector to an equivalent set of colors based on the specified
# ColorBrewer palette. Use the diverging or sequential.
#
# Sequential palettes names:
#  Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples
#  RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
# Diverging palette names:
#  BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
#
# @note Use of this function requires that the \code{RColorBrewer} packages be
#  installed.
# @note This function is based off a Stack Overflow answer by user "Dave X":
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
#  col = numeric_to_colors(iris$Petal.Length, palette = "PRGn", n = 20))
#
# # Use the rainbow color ramp function
# plot(iris[, c("Sepal.Length", "Sepal.Width")], cex = 1.5, pch = 20,
#  col = numeric_to_colors(iris$Petal.Length, col_ramp = rainbow, n = 20))
# }
numeric_to_colors <- function(x, color_scheme = "Blues", n = 15,
                              limits = NULL) {
  if (methods::is(color_scheme, "character") &&
    !requireNamespace("RColorBrewer",
      quietly = TRUE,
      warn.conflicts = FALSE
    )) {
    stop("numeric_to_colors function requires 'RColorBrewer' package")
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
# Creates a color palette with the specified number of colors, interpolating
# ColorBrewer palettes by default.
#
# This function is designed to make it easy to use the ColorBrewer palettes,
# particularly with the qualitative sets, without having to worry about a plot
# not being displayed because the palette didn't have enough colors for the
# number of categories required. Admittedly, you probably shouldn't be using
# the palette in that case, but it's better to see the plot.
#
# Rather than specify a ColorBrewer scheme by name, you can also pass in
# a color ramp function of any kind. For some applicable ramp functions, see
# the \code{Palettes} help page in the \code{grDevices} package (e.g. by
# running the \code{?rainbow} command).
#
# @param ncolors Number of colors desired for the palette.
# @param color_scheme Either the name of a ColorBrewer palette, or a function
#  accepting an integer n as an argument and returning
#  n colors.
# @value A palette with the specified number of colors, interpolated if
#  necessary.
make_palette <- function(ncolors, color_scheme = grDevices::rainbow) {
  if (methods::is(color_scheme, "function")) {
    palette <- color_scheme(ncolors)
  }
  else {
    if (!requireNamespace("RColorBrewer",
      quietly = TRUE,
      warn.conflicts = FALSE
    )) {
      stop("make_palette function requires 'RColorBrewer' package")
    }
    palette <- color_brewer_palette(color_scheme, ncolors)
  }
  palette
}

# Interpolated ColorBrewer Palette
#
# Returns a vector of colors from the specified palette, interpolated if the
# number of requested colors is larger than the number of colors in the
# palette. Sequential and Diverging palettes are suitable for numerical scales.
# The Qualitiative palettes are intended for categorical values.
#
# Sequential palettes names:
#  Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples
#  RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
# Diverging palette names:
#  BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
# Qualitative:
#  Accent Dark2 Paired Pastel1 Pastel2	Set1 Set2	Set3
# @param name Name of the palette.
# @param ncolors Number of colors desired.
# @return Vector of \code{n} colors from the palette.
# @seealso
# More information on ColorBrewer is available at its website,
# \url{http://www.colorbrewer2.org}.
color_brewer_palette <- function(name, ncolors) {
  if (!requireNamespace("RColorBrewer",
    quietly = TRUE,
    warn.conflicts = FALSE
  )) {
    stop("color_brewer_palette function requires 'RColorBrewer' package")
  }
  make_color_brewer_ramp(name)(ncolors)
}

# Interpolated ColorBrewer Ramp
#
# Creates a color ramp function using the ColorBrewer palettes, with
# interpolation if the requested number of colors exceeds the maximum number of
# colors in the palette. Sequential and Diverging palettes are suitable for
# numerical scales. The Qualitiative palettes are intended for categorical
# values.
#
# Sequential palettes names:
#  Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples
#  RdPu Reds YlGn YlGnBu YlOrBr YlOrRd
# Diverging palette names:
#  BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn Spectral
# Qualitative:
#  Accent Dark2 Paired Pastel1 Pastel2	Set1 Set2	Set3
#
# @param name Name of the palette.
# @return Function accepting an integer n as an argument and returning n colors.
#
# @note This function requires the RColorBrewer package to be installed and
#  loaded.
# @seealso
# More information on ColorBrewer is available at its website,
# \url{http://www.colorbrewer2.org}.
make_color_brewer_ramp <- function(name) {
  if (!requireNamespace("RColorBrewer",
    quietly = TRUE,
    warn.conflicts = FALSE
  )) {
    stop("colorBrewerPalette function requires 'RColorBrewer' package")
  }
  if (!name %in% rownames(RColorBrewer::brewer.pal.info)) {
    stop(
      "Unknown ColorBrewer name '", name, "', must be one of ",
      paste(rownames(RColorBrewer::brewer.pal.info), collapse = ", ")
    )
  }

  function(n) {
    max_colors <- RColorBrewer::brewer.pal.info[name, ]$maxcolors
    n <- max(n, 3)
    if (n <= max_colors) {
      RColorBrewer::brewer.pal(n, name)
    }
    else {
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(max_colors, name))(n)
    }
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

# Does PCA and returns the first two components from the X. When X is a 2D
# matrix, this effectively rotates (and potentially reflects) the point set
# so the data aligns along the PCs.
pc_rotate <- function(X) {
  X <- scale(X, center = TRUE, scale = FALSE)
  s <- svd(X, nu = 2, nv = 0)
  s$u %*% diag(c(s$d[1:2]))
}
