# Reproduce the static figures and Plotly widget sources used by the pkgdown
# articles. Run this script from the package root after installing Vizier and
# its suggested plotting packages.

usage <- paste(
  "Usage:",
  "Rscript --vanilla vignettes/articles/reproduce-figures.R",
  "--output-dir DIRECTORY"
)

parse_output_dir <- function(args) {
  output_flags <- which(args == "--output-dir")
  if (
    length(args) != 2L ||
      length(output_flags) != 1L ||
      output_flags[[1L]] != 1L ||
      !nzchar(args[[2L]])
  ) {
    stop(usage, call. = FALSE)
  }
  args[[2L]]
}

required_packages <- c(
  "vizier",
  "paletteer",
  "ggplot2",
  "plotly",
  "htmlwidgets"
)
available <- vapply(
  required_packages,
  requireNamespace,
  logical(1L),
  quietly = TRUE
)
if (any(!available)) {
  stop(
    "Install the packages required to reproduce the article figures: ",
    paste(required_packages[!available], collapse = ", "),
    ".",
    call. = FALSE
  )
}

output_dir <- parse_output_dir(commandArgs(trailingOnly = TRUE))
if (file.exists(output_dir) && !dir.exists(output_dir)) {
  stop("'--output-dir' must name a directory.", call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_dir <- normalizePath(output_dir, mustWork = TRUE)

output_path <- function(filename) {
  file.path(output_dir, filename)
}

save_base_plot <- function(filename, draw, width = 600L, height = 600L) {
  grDevices::png(
    filename = output_path(filename),
    width = width,
    height = height,
    res = 96
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  force(draw)
  invisible(NULL)
}

draw_swatch <- function(colors, title) {
  graphics::par(mar = c(1, 1, 3, 1))
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(-1.4, length(colors) + 1.4),
    ylim = c(-0.3, 1.3),
    xaxs = "i",
    yaxs = "i"
  )
  positions <- seq_along(colors) - 1
  graphics::rect(
    positions,
    0,
    positions + 1,
    1,
    col = as.character(colors),
    border = NA
  )
  graphics::title(main = title)
}

pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)

# Getting Started: base graphics
save_base_plot(
  "embed_ex.png",
  vizier::embed_plot(pca_iris$x, iris)
)

save_base_plot(
  "embed_ex_title.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = grDevices::rainbow,
    title = "iris PCA",
    sub = "rainbow color scheme"
  )
)

save_base_plot(
  "embed_ex_alpha.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = grDevices::rainbow,
    alpha_scale = 0.5
  )
)

my_iris_colors <- grDevices::colorRampPalette(
  c("red", "yellow")
)(nrow(iris))
save_base_plot(
  "embed_ex_colors.png",
  vizier::embed_plot(pca_iris$x, colors = my_iris_colors)
)

save_base_plot(
  "embed_ex_quant.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Petal.Length,
    color_scheme = "RColorBrewer::Blues"
  )
)

save_base_plot(
  "embed_ex_top.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Petal.Length,
    color_scheme = "RColorBrewer::Blues",
    top = 10
  )
)

save_base_plot(
  "embed_ex_ax.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = grDevices::topo.colors,
    equal_axes = TRUE
  )
)

save_base_plot(
  "embed_ex_text.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    text = iris$Species,
    cex = 0.75
  )
)

# Getting Started: ggplot2
iris_ggplot <- vizier::embed_ggplot(
  pca_iris$x,
  iris$Species,
  cex = 2,
  title = "iris PCA"
)
iris_ggplot <- iris_ggplot +
  ggplot2::stat_ellipse(level = 0.8, linewidth = 0.8) +
  ggplot2::labs(
    color = "Species",
    subtitle = "80% confidence ellipses"
  ) +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(
  filename = output_path("embed_ex_ggplot_ellipse.png"),
  plot = iris_ggplot,
  width = 6.25,
  height = 4.6875,
  dpi = 96
)

iris_numeric_ggplot <- vizier::embed_ggplot(
  pca_iris$x,
  iris$Petal.Length,
  cex = 2,
  equal_axes = TRUE,
  title = "iris petal length"
) +
  ggplot2::labs(color = "Petal length") +
  ggplot2::theme_minimal(base_size = 12)
ggplot2::ggsave(
  filename = output_path("embed_ex_ggplot_numeric.png"),
  plot = iris_numeric_ggplot,
  width = 6.25,
  height = 4.6875,
  dpi = 96
)

# Getting Started: Plotly widget sources for bounded manual screenshots
plotly_default <- vizier::embed_plotly(
  pca_iris$x,
  iris$Species,
  color_scheme = grDevices::rainbow
)
htmlwidgets::saveWidget(
  plotly_default,
  file = output_path("embed_ex_plotly.html"),
  selfcontained = FALSE,
  libdir = "plotly-html-dependencies",
  title = "Vizier Plotly example"
)

plotly_tooltip <- vizier::embed_plotly(
  pca_iris$x,
  iris$Species,
  color_scheme = grDevices::rainbow,
  show_legend = FALSE,
  tooltip = paste("Species:", iris$Species)
)
htmlwidgets::saveWidget(
  plotly_tooltip,
  file = output_path("embed_ex_plotly_tooltip.html"),
  selfcontained = FALSE,
  libdir = "plotly-html-dependencies",
  title = "Vizier Plotly tooltip example"
)

# Color Schemes: accepted palette forms and reversal
save_base_plot(
  "embed_ex_topo.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = grDevices::topo.colors
  )
)

species_colors <- c(
  setosa = "black",
  versicolor = "red",
  virginica = "gray"
)
save_base_plot(
  "embed_ex_custom.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = species_colors
  )
)

save_base_plot(
  "embed_ex_okabe_ito.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = "Okabe-Ito"
  )
)

save_base_plot(
  "embed_ex_cb.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = "RColorBrewer::Dark2"
  )
)

save_base_plot(
  "embed_ex_turbo.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = vizier::turbo
  )
)

save_base_plot(
  "embed_ex_turbo_rev.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = vizier::turbo,
    rev = TRUE
  )
)

# Color Schemes: discrete and continuous paletteer behavior
dark2 <- paletteer::paletteer_d("RColorBrewer::Dark2")
save_base_plot(
  "dark2_swatch.png",
  draw_swatch(dark2, "RColorBrewer Dark2")
)

save_base_plot(
  "embed_dark2.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = "RColorBrewer::Dark2",
    cex = 2,
    title = "RColorBrewer Dark2"
  )
)

save_base_plot(
  "embed_dark2c.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = "RColorBrewer::Dark2::c",
    cex = 2,
    title = "RColorBrewer Dark2 (continuous)"
  )
)

jcolors_rainbow <- paletteer::paletteer_d("jcolors::rainbow")
save_base_plot(
  "rainbow_swatch.png",
  draw_swatch(jcolors_rainbow, "jcolors rainbow")
)

save_base_plot(
  "embed_jcrainbow.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = "jcolors::rainbow",
    cex = 2,
    title = "jcolors rainbow"
  )
)

save_base_plot(
  "embed_jcrainbowc.png",
  vizier::embed_plot(
    pca_iris$x,
    iris$Species,
    color_scheme = "jcolors::rainbow::c",
    cex = 2,
    title = "jcolors rainbow (continuous)"
  )
)

automated_outputs <- c(
  "embed_ex.png",
  "embed_ex_title.png",
  "embed_ex_alpha.png",
  "embed_ex_colors.png",
  "embed_ex_quant.png",
  "embed_ex_top.png",
  "embed_ex_ax.png",
  "embed_ex_text.png",
  "embed_ex_ggplot_ellipse.png",
  "embed_ex_ggplot_numeric.png",
  "embed_ex_plotly.html",
  "embed_ex_plotly_tooltip.html",
  "embed_ex_topo.png",
  "embed_ex_custom.png",
  "embed_ex_okabe_ito.png",
  "embed_ex_cb.png",
  "embed_ex_turbo.png",
  "embed_ex_turbo_rev.png",
  "dark2_swatch.png",
  "embed_dark2.png",
  "embed_dark2c.png",
  "rainbow_swatch.png",
  "embed_jcrainbow.png",
  "embed_jcrainbowc.png"
)
missing_outputs <- automated_outputs[
  !file.exists(output_path(automated_outputs))
]
if (length(missing_outputs) > 0L) {
  stop(
    "Figure reproduction did not create: ",
    paste(missing_outputs, collapse = ", "),
    ".",
    call. = FALSE
  )
}

message("Generated 22 PNG figures and two Plotly widget sources in:")
message(output_dir)
message("")
message("Manual Plotly capture recipe (100% browser zoom):")
message(
  "1. Open embed_ex_plotly.html, size the plot region to 462 x 456 px, ",
  "hover iris row 107, and capture embed_ex_plotly.png."
)
message(
  "2. Open embed_ex_plotly_tooltip.html at the same size, hover iris row 61, ",
  "and capture embed_ex_plotly_tooltip.png."
)
message("Keep the Plotly mode bar visible in both captures.")
