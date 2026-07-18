get_colors <- function(
  x,
  color_scheme = NULL,
  num_colors = 15,
  limits = NULL,
  top = NULL,
  colors = NULL,
  alpha_scale = 1,
  NA_color = NULL,
  rev = FALSE,
  numeric_ok = FALSE,
  fallback_color_scheme = grDevices::rainbow,
  verbose = FALSE
) {
  n <- if (!is.null(colors)) {
    length(colors)
  } else if (methods::is(x, "data.frame")) {
    nrow(x)
  } else if (!is.null(x)) {
    length(x)
  } else {
    num_colors
  }
  res <- resolve_colors(
    x = x,
    colors = colors,
    n = n,
    color_scheme = color_scheme,
    num_colors = num_colors,
    limits = limits,
    top = top,
    alpha_scale = alpha_scale,
    NA_color = NA_color,
    rev = rev,
    numeric_ok = numeric_ok,
    fallback_color_scheme = fallback_color_scheme,
    verbose = verbose
  )
  grDevices::adjustcolor(res$colors, alpha.f = alpha_scale)
}

# Resolve every color-related public input once so renderers do not make their
# own classification, palette, reversal, or selection decisions.
resolve_colors <- function(
  x,
  colors,
  n,
  color_scheme = NULL,
  num_colors = 15,
  limits = NULL,
  top = NULL,
  alpha_scale = 1,
  NA_color = NULL,
  rev = FALSE,
  numeric_ok = FALSE,
  fallback_color_scheme = grDevices::rainbow,
  verbose = FALSE,
  clip_limit_values = TRUE
) {
  validate_alpha_scale(alpha_scale)
  validate_logical_scalar(rev, "'rev'")
  validate_logical_scalar(clip_limit_values, "'clip_limit_values'")
  if (!is.null(limits)) {
    validate_numeric_limits(limits)
  }
  if (
    !is.null(top) &&
      (!is.null(colors) || methods::is(x, "data.frame") || !is.numeric(x))
  ) {
    stop("'top' is only supported for a numeric 'x' vector.", call. = FALSE)
  }

  if (!is.null(colors)) {
    colors <- recycle_input(colors, n, "'colors'")
    return(color_spec(
      kind = "identity",
      colors = replace_na_color(colors, NA_color),
      keep = rep(TRUE, n)
    ))
  }

  source <- resolve_color_source(x, n, numeric_ok, verbose)
  if (source$kind == "identity") {
    return(color_spec(
      kind = "identity",
      colors = replace_na_color(source$values, NA_color),
      keep = rep(TRUE, n)
    ))
  }

  if (source$kind == "row") {
    palette <- make_palette(n, color_scheme, verbose = verbose)
    if (rev) {
      palette <- rev(palette)
    }
    return(color_spec(
      kind = "row",
      colors = replace_na_color(palette, NA_color),
      palette = palette,
      keep = rep(TRUE, n)
    ))
  }

  if (source$kind == "discrete") {
    labels <- source$values
    category_names <- category_levels(labels)
    palette <- categorical_palette(
      category_names,
      color_scheme = color_scheme,
      rev = rev,
      verbose = verbose
    )
    mapped <- unname(palette[as.character(labels)])
    return(color_spec(
      kind = "discrete",
      values = labels,
      colors = replace_na_color(mapped, NA_color),
      palette = palette,
      keep = rep(TRUE, n),
      labels = labels
    ))
  }

  validate_num_colors(num_colors)
  values <- source$values
  finite <- is.finite(values)
  if (!is.null(top)) {
    validate_top(top, sum(finite))
    keep <- rep(FALSE, n)
    selected <- order(values[finite], decreasing = TRUE, method = "radix")
    keep[which(finite)[selected[seq_len(top)]]] <- TRUE
  } else {
    keep <- rep(TRUE, n)
  }

  color_limits <- numeric_color_limits(values, limits)
  mapped_values <- values
  if (!is.null(color_limits)) {
    outside <- finite & (values < color_limits[1] | values > color_limits[2])
    if (clip_limit_values) {
      mapped_values[outside & values < color_limits[1]] <- color_limits[1]
      mapped_values[outside & values > color_limits[2]] <- color_limits[2]
    } else {
      mapped_values[outside] <- NA_real_
    }
  }
  mapped_values[!is.finite(mapped_values)] <- NA_real_
  palette <- make_continuous_palette(
    num_colors,
    color_scheme,
    verbose = verbose
  )
  if (rev) {
    palette <- rev(palette)
  }
  mapped <- numeric_to_colors(
    mapped_values,
    palette,
    n = length(palette),
    limits = color_limits
  )
  mapped[!keep] <- NA_character_
  if (!is.null(NA_color)) {
    mapped[is.na(mapped) & keep] <- NA_color
  }
  color_spec(
    kind = "continuous",
    values = values,
    colors = mapped,
    palette = palette,
    limits = color_limits,
    keep = keep,
    missing = !finite,
    mapped_values = mapped_values,
    labels = values
  )
}

color_spec <- function(
  kind,
  colors,
  values = NULL,
  palette = NULL,
  limits = NULL,
  keep,
  missing = rep(FALSE, length(colors)),
  mapped_values = NULL,
  labels = NULL
) {
  list(
    kind = kind,
    values = values,
    colors = colors,
    palette = palette,
    limits = limits,
    keep = keep,
    missing = missing,
    mapped_values = mapped_values,
    labels = labels
  )
}

resolve_color_source <- function(x, n, numeric_ok, verbose) {
  if (is.null(x)) {
    return(list(kind = "row"))
  }
  if (methods::is(x, "data.frame")) {
    if (nrow(x) != n) {
      stop("'x' data frame must have one row per coordinate.", call. = FALSE)
    }
    color_name <- last_color_column_name(x)
    if (!is.null(color_name)) {
      return(list(kind = "identity", values = x[[color_name]]))
    }
    factor_name <- last_factor_column_name(x)
    if (!is.null(factor_name)) {
      return(list(kind = "discrete", values = x[[factor_name]]))
    }
    character_name <- last_character_column_name(x)
    if (!is.null(character_name) && is_factorish(x[[character_name]])) {
      return(list(kind = "discrete", values = x[[character_name]]))
    }
    if (numeric_ok) {
      numeric_name <- last_numeric_column_name(x)
      if (!is.null(numeric_name)) {
        return(list(kind = "continuous", values = x[[numeric_name]]))
      }
    }
    return(list(kind = "row"))
  }
  if (length(x) != n) {
    stop("'x' must have one value per coordinate.", call. = FALSE)
  }
  if (is.numeric(x)) {
    return(list(kind = "continuous", values = x))
  }
  if (is_color_column(x)) {
    return(list(kind = "identity", values = x))
  }
  if (is.factor(x) || is.character(x)) {
    return(list(kind = "discrete", values = x))
  }
  list(kind = "row")
}

category_levels <- function(x) {
  if (is.factor(x)) {
    return(levels(x))
  }
  levels(as.factor(x))
}

categorical_palette <- function(category_names, color_scheme, rev, verbose) {
  if (is_named_palette(color_scheme)) {
    palette <- color_scheme[category_names]
    if (anyNA(palette)) {
      missing <- category_names[is.na(palette)]
      stop(
        "Named 'color_scheme' is missing observed categories: ",
        paste(missing, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  } else {
    palette <- make_palette(
      length(category_names),
      color_scheme,
      verbose = verbose
    )
    names(palette) <- category_names
  }
  if (rev) {
    palette[] <- rev(unname(palette))
  }
  palette
}

is_named_palette <- function(color_scheme) {
  if (is.null(color_scheme) || methods::is(color_scheme, "function")) {
    return(FALSE)
  }
  palette_names <- names(color_scheme)
  if (is.null(palette_names)) {
    return(FALSE)
  }
  if (
    anyNA(palette_names) ||
      any(palette_names == "") ||
      anyDuplicated(palette_names)
  ) {
    stop(
      "'color_scheme' names must be complete, unique, and non-empty.",
      call. = FALSE
    )
  }
  TRUE
}

replace_na_color <- function(colors, NA_color) {
  if (!is.null(NA_color)) {
    colors[is.na(colors)] <- NA_color
  }
  colors
}

recycle_input <- function(x, n, name) {
  if (length(x) != 1 && length(x) != n) {
    stop(
      name,
      " must have length 1 or one value per coordinate.",
      call. = FALSE
    )
  }
  rep(x, length.out = n)
}

validate_alpha_scale <- function(alpha_scale) {
  if (
    !is.numeric(alpha_scale) ||
      length(alpha_scale) != 1 ||
      is.na(alpha_scale) ||
      !is.finite(alpha_scale) ||
      alpha_scale < 0 ||
      alpha_scale > 1
  ) {
    stop("'alpha_scale' must be a finite number in [0, 1].", call. = FALSE)
  }
}

validate_logical_scalar <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(name, " must be a single non-missing logical value.", call. = FALSE)
  }
}

validate_top <- function(top, n_finite) {
  if (
    !is.numeric(top) ||
      length(top) != 1 ||
      is.na(top) ||
      !is.finite(top) ||
      top < 1 ||
      top != as.integer(top) ||
      top > n_finite
  ) {
    stop(
      "'top' must be a positive integer no greater than the number of finite values.",
      call. = FALSE
    )
  }
}

numeric_color_limits <- function(x, limits) {
  if (!is.null(limits)) {
    return(validate_numeric_limits(limits))
  }
  finite_x <- x[is.finite(x)]
  if (length(finite_x) == 0) {
    return(NULL)
  }
  range(finite_x)
}


# Given a data frame or a vector, return a vector of colors appropriately
# mapped to the color scheme.
# If `x` is a vector, it can either be a vector of colors, a factor vector
# or factor-like character vector (in which case each level is mapped to a
# color), or a numeric vector (in which case the range is mapped linearly). If
# `x` is a data frame, then it is checked for a color column. If there
# isn't one, a factor column (or character column that can be treated like a
# factor) is looked for. If there's more than one suitable column, the last
# found column is used. Numeric columns aren't searched for in the data frame
# case.
color_helper <- function(
  x,
  color_scheme = NULL,
  num_colors = 15,
  limits = NULL,
  top = NULL,
  numeric_ok = FALSE,
  fallback_color_scheme = grDevices::rainbow,
  verbose = FALSE
) {
  if (methods::is(x, "data.frame")) {
    res <- color_helper_df(
      x,
      color_scheme = color_scheme,
      numeric_ok = numeric_ok,
      fallback_color_scheme = fallback_color_scheme,
      verbose = verbose
    )
  } else {
    res <- color_helper_column(
      x,
      color_scheme = color_scheme,
      num_colors = num_colors,
      limits = limits,
      top = top,
      verbose = verbose
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
# if numeric_ok is TRUE, then if other ways to find colors, before going with
# one color per point, try to map the last numeric column to a continuous
# color scheme. Default is FALSE because if passing in a mixed dataframe of
# labels and data, it's likely that the numeric columns are not meant to be
# interpreted as a continuous color scale (they're the raw data).
# Otherwise, color each point individually.
# In the latter two cases where we can't find a categorical-like column, the
# `fallback_color_scheme` will be used, so it probably should be a continuous
# color scheme
# @note R considers numbers to be acceptable colors because `col2rgb()`
# interprets them as indexes into a palette. Columns of numbers are NOT treated
# as colors by color_helper. Stick with color names (e.g. "goldenrod") or
# rgb strings (e.g. "#140000" or "#140000FF" if including alpha values).
# If ret_labels is TRUE, return the column used for the mapping
color_helper_df <- function(
  df,
  color_scheme = NULL,
  numeric_ok = FALSE,
  fallback_color_scheme = grDevices::rainbow,
  verbose = FALSE
) {
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
    palette <- factor_to_palette(
      labels,
      color_scheme = color_scheme,
      verbose = verbose
    )
    return(list(labels = labels, palette = palette))
  }

  # Is there something factorish?
  label_name <- last_character_column_name(df)
  if (!is.null(label_name) && is_factorish(df[[label_name]])) {
    if (verbose) {
      message(
        "Found a character column '",
        label_name,
        "' for mapping to colors"
      )
    }
    labels <- df[[label_name]]
    palette <- factor_to_palette(
      labels,
      color_scheme = color_scheme,
      verbose = verbose
    )
    return(list(labels = labels, palette = palette))
  }

  # Either a numeric or one-point-per color scheme here
  # use fallback_color_scheme from here on out
  if (numeric_ok) {
    numeric_name <- last_numeric_column_name(df)
    if (!is.null(numeric_name)) {
      if (verbose) {
        message(
          "Found a numeric column '",
          numeric_name,
          "' for mapping to colors"
        )
      }
      colors <- numeric_to_colors(
        df[[numeric_name]],
        color_scheme = fallback_color_scheme
      )
      return(list(colors = colors))
    }
  }

  # use one color per point
  if (verbose) {
    message("Using one color per point")
  }
  colors <- make_palette(
    ncolors = nrow(df),
    color_scheme = fallback_color_scheme
  )
  list(colors = colors, labels = labels)
}

color_helper_column <- function(
  x,
  color_scheme,
  num_colors = 15,
  limits = NULL,
  top = NULL,
  verbose = FALSE
) {
  # Is this a color column - return as-is
  if (is_color_column(x)) {
    return(list(colors = x))
  }

  # Is it numeric - map to continuous palette
  if (is.numeric(x)) {
    colors <- numeric_to_colors(
      x,
      color_scheme = color_scheme,
      n = num_colors,
      limits = limits
    )
    if (!is.null(top)) {
      svec <- sort(x, decreasing = TRUE)
      colors[x < svec[top]] <- NA
    }
    return(list(colors = colors))
  }

  # Is it a factor - map to palette (which should be categorical)
  if (is.factor(x)) {
    palette <- factor_to_palette(
      x,
      color_scheme = color_scheme,
      verbose = verbose
    )
    return(list(labels = x, palette = palette))
  }

  # Probably a column of characters, can they be treated as a factor?
  if (is_factorish(x)) {
    palette <- factor_to_palette(
      as.factor(x),
      color_scheme = color_scheme,
      verbose = verbose
    )
    return(list(labels = x, palette = palette))
  }

  # Otherwise one color per point (doesn't really matter what the palette is!)
  list(colors = make_palette(ncolors = length(x), color_scheme = color_scheme))
}

# Map a vector of factor levels, x, to a vector of colors taken from either
# a color ramp function, color scheme name or existing palette
factor_to_colors <- function(x, color_scheme = NULL, verbose = FALSE) {
  factor_to_palette(x, color_scheme, verbose = verbose)[as.character(x)]
}

# Map a vector of factor levels, x, to a palette based on the specified
# color scheme
factor_to_palette <- function(x, color_scheme = NULL, verbose = FALSE) {
  x <- as.factor(x)
  category_names <- levels(x)
  ncolors <- length(category_names)
  stats::setNames(
    make_palette(
      ncolors = ncolors,
      color_scheme = color_scheme,
      verbose = verbose
    ),
    category_names
  )
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
#  <http://stackoverflow.com/a/18749392>
#
# @param x Numeric vector.
# @param name Name of the ColorBrewer palette.
# @param n Number of unique colors to map values in `x` to.
# @param limits The range that the colors should map over. If not specified,
#  then the range of `x`. This is useful if there is some external
#  absolute scale that should be used.
# @seealso
# More information on ColorBrewer is available at its website,
# <http://www.colorbrewer2.org>.
# @examples
# if (interactive()) {
# # Plot Iris dataset sepal width vs length, colored by petal length, using
# # 20 colors ranging from Purple to Green (PRGn):
# plot(iris[, c("Sepal.Length", "Sepal.Width")], cex = 1.5, pch = 20,
#  col = numeric_to_colors(iris$Petal.Length, color_scheme = "RColorBrewer::PRGn", n = 20))
#
# # Use the rainbow color ramp function
# plot(iris[, c("Sepal.Length", "Sepal.Width")], cex = 1.5, pch = 20,
#  col = numeric_to_colors(iris$Petal.Length, color_scheme = rainbow, n = 20))
# }
numeric_to_colors <- function(
  x,
  color_scheme = "RColorBrewer::Blues",
  n = NULL,
  limits = NULL
) {
  if (is.null(n)) {
    n <- length(x)
  }

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || !is.finite(n) || n < 1) {
    stop("'n' must be a positive finite number.", call. = FALSE)
  }
  n <- as.integer(n)

  if (is.null(limits)) {
    finite_x <- x[is.finite(x)]
    if (length(finite_x) == 0) {
      return(rep(NA_character_, length(x)))
    }
    limits <- range(finite_x)
  }

  limits <- validate_numeric_limits(limits)

  pal <- make_palette(ncolors = n, color_scheme = color_scheme)
  colors <- rep(NA_character_, length(x))
  ok <- is.finite(x)

  if (limits[1] == limits[2]) {
    colors[ok] <- pal[[ceiling(length(pal) / 2)]]
    return(colors)
  }

  breaks <- seq(limits[1], limits[2], length.out = length(pal) + 1)
  colors[ok] <- pal[findInterval(x[ok], breaks, all.inside = TRUE)]
  colors
}

validate_numeric_limits <- function(limits) {
  if (
    !is.numeric(limits) ||
      length(limits) != 2 ||
      anyNA(limits) ||
      !all(is.finite(limits))
  ) {
    stop("'limits' must contain two finite numeric values.", call. = FALSE)
  }

  if (limits[1] > limits[2]) {
    stop("'limits' must be in increasing order.", call. = FALSE)
  }

  limits
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

last_numeric_column_name <- function(df) {
  numeric_name <- NULL
  numeric_names <- filter_column_names(df, is.numeric)
  if (length(numeric_names) > 0) {
    numeric_name <- numeric_names[length(numeric_names)]
  }
  numeric_name
}


# returns TRUE if vector x consists of colors
is_color_column <- function(x) {
  !is.numeric(x) && all(is.na(x) | is_color(x))
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
# <http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation>
# @note numeric values are always seen as being valid colors!
is_color <- function(x) {
  vapply(
    x,
    function(X) {
      tryCatch(
        is.matrix(grDevices::col2rgb(X)),
        error = function(e) {
          FALSE
        }
      )
    },
    logical(1)
  )
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
