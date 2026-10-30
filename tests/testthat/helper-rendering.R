# Capture the public base renderer's plot arguments without opening a device.
base_plot_args <- function(...) {
  result <- NULL
  testthat::local_mocked_bindings(
    plot = function(x, ...) result <<- c(list(coords = x), list(...)),
    .package = "base"
  )
  embed_plot(...)
  result
}

base_colors <- function(...) {
  base_plot_args(...)$col
}
