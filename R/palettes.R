# Color Palette with Specified Number of Colors
#
# Returns a palette with the specified size, based on an existing palette,
# color scheme name or color ramp function.
make_palette <- function(ncolors, color_scheme = NULL, verbose = FALSE) {
  if (is.null(color_scheme)) {
    palette <- make_polychrome_palette(ncolors)
  } else if (methods::is(color_scheme, "function")) {
    palette <- color_scheme(ncolors)
  } else {
    palette <- make_palette_function(color_scheme, verbose = verbose)(ncolors)
  }
  as.character(palette)
}

# something a bit like the Python package glasbey provides
# a categorical palette with easily distinguishable colors
make_polychrome_palette <- function(ncolors) {
  as.vector(Polychrome::createPalette(
    ncolors + 2,
    seedcolors = c("#ffffff", "#000000"),
    range = c(10, 90)
  )[-(1:2)])
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
      } else {
        palette <- name[seq_len(n)]
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
    } else {
      stop(
        "Bad palette name '",
        name,
        "'. ",
        "Should be in format: <package>::<palette>[::<d|c>]"
      )
    }
  }

  package_name <- split_res[1]
  palette_name <- split_res[2]
  if (length(split_res) == 3) {
    type <- switch(
      tolower(split_res[3]),
      "c" = "continuous",
      "continuous" = "continuous",
      "d" = "discrete",
      "discrete" = "discrete",
      stop(
        "Palette type must be one of 'c', 'continuous', 'd', or 'discrete'",
        call. = FALSE
      )
    )
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
        "Unknown palette '",
        palette_name,
        "' for package '",
        package_name,
        "'"
      )
    }
  }

  pal_fn <- switch(
    as.character(pal$type),
    "r" = function(package_name, palette_name, n) {
      grDevices::palette.colors(n = n, palette = palette_name)
    },
    "c" = function(package_name, palette_name, n) {
      pack_and_pal <- paste0(package_name, "::", palette_name)
      forceAndCall(2, paletteer::paletteer_c, pack_and_pal, n)
    },
    "d" = function(package_name, palette_name, n) {
      pack_and_pal <- paste0(package_name, "::", palette_name)
      if (is.null(type)) {
        forceAndCall(2, paletteer::paletteer_d, pack_and_pal, n)
      } else {
        forceAndCall(
          3,
          paletteer::paletteer_d,
          pack_and_pal,
          n = n,
          type = type
        )
      }
    },
    "dynamic" = function(package_name, palette_name, n) {
      pack_and_pal <- paste0(package_name, "::", palette_name)
      forceAndCall(2, paletteer::paletteer_dynamic, pack_and_pal, n)
    }
  )
  max_colors <- pal$length
  function(n) {
    ncols <- n
    if (n > max_colors) {
      if (verbose) {
        message(
          "Interpolating palette for ",
          n,
          " colors from ",
          max_colors
        )
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

is_r_palette <- function(name) {
  exists("palette.pals", where = "package:grDevices") &&
    name %in% grDevices::palette.pals()
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
    package = all_packages,
    palette = all_palettes,
    length = all_lengths,
    type = all_types
  )
}
