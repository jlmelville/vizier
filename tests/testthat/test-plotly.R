test_that("embed_plotly accepts paletteer color vectors", {
  testthat::skip_if_not_installed("plotly")

  pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)

  p <- embed_plotly(
    pca_iris$x,
    iris,
    color_scheme = "jcolors::rainbow::c",
    cex = 2,
    title = "jcolors rainbow (continuous)"
  )

  expect_s3_class(p, "plotly")
  expect_s3_class(plotly::plotly_build(p), "plotly")
})
