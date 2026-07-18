test_that("embed_ggplot reports its optional dependency clearly", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE) FALSE,
    .package = "base"
  )

  expect_error(
    embed_ggplot(cbind(1, 2)),
    'Package "ggplot2" must be installed to use embed_ggplot\\(\\).',
    fixed = FALSE
  )
})

test_that("embed_ggplot returns native layers and scales for resolved colors", {
  testthat::skip_if_not_installed("ggplot2")

  coords <- cbind(1:3, 4:6)
  cases <- list(
    discrete = embed_ggplot(
      coords,
      factor(c("a", "b", "a")),
      color_scheme = c(a = "#AA0000", b = "#0055AA")
    ),
    continuous = embed_ggplot(coords, c(1, 2, 3), limits = c(1, 3)),
    identity = embed_ggplot(
      coords,
      colors = c("#AA0000", "#0055AA", "#118833")
    ),
    row = embed_ggplot(coords)
  )

  expected_scale_classes <- c(
    discrete = "ScaleDiscrete",
    continuous = "ScaleContinuous",
    identity = "ScaleDiscreteIdentity",
    row = "ScaleDiscreteIdentity"
  )
  for (name in names(cases)) {
    p <- cases[[name]]
    built <- ggplot2::ggplot_build(p)

    expect_s3_class(p, "ggplot")
    expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
    expect_s3_class(
      p$scales$get_scales("colour"),
      expected_scale_classes[[name]]
    )
    expect_equal(nrow(built$data[[1]]), 3)
  }

  discrete_colors <- ggplot2::ggplot_build(cases$discrete)$data[[1]]$colour
  expect_equal(discrete_colors, c("#AA0000", "#0055AA", "#AA0000"))
  continuous_scale <- cases$continuous$scales$get_scales("colour")
  expect_equal(continuous_scale$limits, c(1, 3))
})

test_that("embed_ggplot renders missing values with the resolved color", {
  testthat::skip_if_not_installed("ggplot2")

  coords <- cbind(1:3, 4:6)
  cases <- list(
    discrete = embed_ggplot(
      coords,
      factor(c("a", NA, "b")),
      NA_color = "grey70"
    ),
    continuous = embed_ggplot(
      coords,
      c(1, NA, 3),
      NA_color = "grey70"
    ),
    identity = embed_ggplot(
      coords,
      colors = c("#AA0000", NA, "#118833"),
      NA_color = "grey70"
    )
  )

  for (p in cases) {
    built <- expect_no_warning(ggplot2::ggplot_build(p))
    expect_identical(built$data[[1]]$colour[[2]], "grey70")
  }
})

test_that("embed_ggplot translates text, coordinates, titles, and hidden axes", {
  testthat::skip_if_not_installed("ggplot2")

  p <- embed_ggplot(
    cbind(c(0, 2, 4), c(10, 20, 30)),
    c(1, 2, 3),
    text = c("one", "two", "three"),
    title = "title",
    sub = "subtitle",
    xlim = c(-1, 5),
    ylim = c(9, 31),
    show_axes = FALSE
  )
  fixed <- embed_ggplot(
    cbind(c(0, 2, 4), c(10, 20, 30)),
    colors = "#AA0000",
    equal_axes = TRUE
  )
  built <- ggplot2::ggplot_build(p)

  expect_s3_class(p$layers[[1]]$geom, "GeomText")
  expect_equal(as.character(built$data[[1]]$label), c("one", "two", "three"))
  expect_identical(p$labels$title, "title")
  expect_identical(p$labels$subtitle, "subtitle")
  expect_equal(p$coordinates$limits$x, c(-1, 5))
  expect_equal(p$coordinates$limits$y, c(9, 31))
  expect_equal(fixed$coordinates$ratio, 1)
  expect_equal(
    fixed$coordinates$limits$x,
    range(c(0, 2, 4, 10, 20, 30))
  )
  expect_equal(
    fixed$coordinates$limits$y,
    range(c(0, 2, 4, 10, 20, 30))
  )
  expect_s3_class(p$theme$axis.text, "element_blank")
  expect_s3_class(p$theme$axis.title, "element_blank")
  expect_s3_class(p$theme$axis.ticks, "element_blank")
})

test_that("embed_ggplot retains only selected numeric rows", {
  testthat::skip_if_not_installed("ggplot2")

  p <- embed_ggplot(cbind(1:4, 5:8), c(2, 9, 9, 1), top = 2)
  built <- ggplot2::ggplot_build(p)

  expect_equal(built$data[[1]]$x, c(2, 3))
})
