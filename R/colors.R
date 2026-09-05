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
  verbose = FALSE,
  clip_limit_values = TRUE,
  map_colors = TRUE
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

  source <- resolve_color_source(x, n)
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
    observed <- category_names[category_names %in% as.character(labels)]
    if (is_named_palette(color_scheme)) {
      category_names <- observed
    }
    palette <- categorical_palette(
      category_names,
      color_scheme = color_scheme,
      rev = rev,
      verbose = verbose
    )
    palette <- palette[observed]
    mapped <- if (map_colors) unname(palette[as.character(labels)]) else NULL
    return(color_spec(
      kind = "discrete",
      values = labels,
      colors = replace_na_color(mapped, NA_color),
      palette = palette,
      keep = rep(TRUE, n),
      labels = labels
    ))
  }

  if (is.null(num_colors)) {
    num_colors <- length(source$values)
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
  mapped <- NULL
  if (map_colors) {
    mapped <- numeric_to_colors(mapped_values, palette, limits = color_limits)
    mapped[!keep] <- NA_character_
    if (!is.null(NA_color)) {
      mapped[is.na(mapped) & keep] <- NA_color
    }
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

resolve_color_source <- function(x, n) {
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
    if (!is.null(character_name)) {
      return(list(kind = "discrete", values = x[[character_name]]))
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


# Map numeric values using the already resolved palette and limits.
numeric_to_colors <- function(x, palette, limits) {
  if (is.null(limits)) {
    return(rep(NA_character_, length(x)))
  }
  colors <- rep(NA_character_, length(x))
  ok <- is.finite(x)

  if (limits[1] == limits[2]) {
    colors[ok] <- palette[[ceiling(length(palette) / 2)]]
    return(colors)
  }

  breaks <- seq(limits[1], limits[2], length.out = length(palette) + 1)
  colors[ok] <- palette[findInterval(x[ok], breaks, all.inside = TRUE)]
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
# column which is factor-like character data, or NULL if none is suitable.
last_character_column_name <- function(df) {
  char_name <- NULL
  char_names <- filter_column_names(df, is_factorish)
  if (length(char_names) > 0) {
    char_name <- char_names[length(char_names)]
  }
  char_name
}

# returns TRUE if vector x consists of colors
is_color_column <- function(x) {
  if (is.numeric(x)) {
    return(FALSE)
  }
  candidates <- unique(x[!is.na(x)])
  length(candidates) > 0 && all(is_color(candidates))
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
