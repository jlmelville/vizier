# Capture the public base renderer's final colors without opening a device.
base_colors <- function(...) {
  result <- NULL
  testthat::local_mocked_bindings(
    plot = function(..., col) result <<- col,
    .package = "base"
  )
  embed_plot(...)
  result
}
