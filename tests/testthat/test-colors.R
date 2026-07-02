test_that("character labels map to non-missing colors", {
  labels <- c("a", "b", "a")

  colors <- vizier:::get_colors(
    labels,
    color_scheme = c("red", "blue")
  )

  expect_false(anyNA(colors))
  expect_identical(colors[[1]], colors[[3]])
  expect_false(identical(colors[[1]], colors[[2]]))
})

test_that("data-frame character labels map to colors", {
  meta <- data.frame(label = c("a", "b", "a"))

  res <- vizier:::color_helper_df(
    meta,
    color_scheme = c("red", "blue")
  )
  colors <- vizier:::get_colors(
    meta,
    color_scheme = c("red", "blue")
  )

  expect_named(res$palette, c("a", "b"))
  expect_false(anyNA(colors))
  expect_identical(colors[[1]], colors[[3]])
  expect_false(identical(colors[[1]], colors[[2]]))
})

test_that("factor labels use level names for palette lookup", {
  labels <- factor(c("b", "a", "b"), levels = c("b", "a"))

  palette <- vizier:::factor_to_palette(
    labels,
    color_scheme = c("red", "blue")
  )
  colors <- vizier:::get_colors(
    labels,
    color_scheme = c("red", "blue")
  )

  expect_named(palette, c("b", "a"))
  expect_identical(unname(palette), c("red", "blue"))
  expect_identical(unname(colors[[1]]), grDevices::adjustcolor("red"))
  expect_identical(unname(colors[[2]]), grDevices::adjustcolor("blue"))
  expect_identical(colors[[1]], colors[[3]])
})

test_that("custom palettes return the requested length", {
  expect_identical(
    vizier:::make_palette(2, c("red", "green", "blue")),
    c("red", "green")
  )
})

test_that("paletteer shorthand supports continuous and discrete types", {
  expect_length(vizier:::make_palette(3, "RColorBrewer::Dark2::c"), 3)
  expect_length(vizier:::make_palette(3, "RColorBrewer::Dark2::continuous"), 3)
  expect_length(vizier:::make_palette(3, "RColorBrewer::Dark2::d"), 3)
  expect_length(vizier:::make_palette(3, "RColorBrewer::Dark2::discrete"), 3)
})

test_that("numeric color mapping handles missing and non-finite values", {
  colors <- vizier:::numeric_to_colors(
    c(1, NA, Inf, -Inf, 2),
    color_scheme = c("red", "blue"),
    n = 2
  )

  expect_false(anyNA(colors[c(1, 5)]))
  expect_true(all(is.na(colors[c(2, 3, 4)])))
})

test_that("numeric color mapping handles constant finite values", {
  colors <- vizier:::numeric_to_colors(
    c(5, 5, NA),
    color_scheme = c("red", "blue", "green"),
    n = 3
  )

  expect_identical(colors[[1]], "blue")
  expect_identical(colors[[2]], "blue")
  expect_true(is.na(colors[[3]]))
})

test_that("numeric color mapping validates limits", {
  expect_error(
    vizier:::numeric_to_colors(c(1, 2), limits = c(1, NA)),
    "finite numeric values"
  )
  expect_error(
    vizier:::numeric_to_colors(c(1, 2), limits = c(2, 1)),
    "in increasing order"
  )
})
