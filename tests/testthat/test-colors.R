test_that("public base rendering preserves categorical selection and ordering", {
  coords <- cbind(1:3, 4:6)
  palette <- c("red", "blue")
  for (labels in list(c("a", "b", "a"), data.frame(label = c("a", "b", "a")))) {
    expect_identical(
      base_colors(coords, labels, color_scheme = palette),
      grDevices::adjustcolor(c("red", "blue", "red"))
    )
  }
  meta <- data.frame(
    label = factor(c("a", "b", "a")),
    color1 = c("red", "blue", "red"),
    color2 = c("#111111", "#222222", "#333333"),
    value = c(3, 1, 2)
  )
  expect_identical(
    base_colors(coords, meta),
    grDevices::adjustcolor(meta$color2)
  )
  labels <- factor(c("b", "a", "b"), levels = c("b", "a"))
  expect_identical(
    base_colors(coords, labels, color_scheme = palette),
    grDevices::adjustcolor(c("red", "blue", "red"))
  )
  meta <- data.frame(id = paste0("row", 1:3), value = c(3, 1, 2))
  expect_identical(base_colors(coords, meta), base_colors(coords))
})

test_that("custom palettes return the requested length", {
  expect_identical(
    vizier:::make_palette(2, c("red", "green", "blue")),
    c("red", "green")
  )
})

test_that("default categorical palette uses Polychrome 36 when possible", {
  expect_identical(
    vizier:::make_palette(10),
    unname(grDevices::palette.colors(10, palette = "Polychrome 36"))
  )
  expect_identical(
    vizier:::make_palette(36),
    unname(grDevices::palette.colors(36, palette = "Polychrome 36"))
  )
})

test_that("default categorical palette falls back for more than 36 colors", {
  expect_length(suppressWarnings(vizier:::make_palette(37)), 37)
})

test_that("paletteer shorthand supports continuous and discrete types", {
  expect_length(vizier:::make_palette(3, "RColorBrewer::Dark2::c"), 3)
  expect_length(vizier:::make_palette(3, "RColorBrewer::Dark2::continuous"), 3)
  expect_length(vizier:::make_palette(3, "RColorBrewer::Dark2::d"), 3)
  expect_length(vizier:::make_palette(3, "RColorBrewer::Dark2::discrete"), 3)
})

test_that("public numeric colors handle non-finite and constant values", {
  coords <- cbind(1:5, 6:10)
  colors <- base_colors(
    coords,
    c(1, NA, Inf, -Inf, 2),
    color_scheme = c("red", "blue"),
    num_colors = 2
  )
  expect_identical(colors, grDevices::adjustcolor(c("red", NA, NA, NA, "blue")))
  colors <- base_colors(
    coords[1:3, ],
    c(5, 5, NA),
    color_scheme = c("red", "blue", "green"),
    num_colors = 3
  )
  expect_identical(colors, grDevices::adjustcolor(c("blue", "blue", NA)))
  colors <- base_colors(coords[1:3, ], c(NA_real_, Inf, -Inf))
  expect_identical(colors, grDevices::adjustcolor(rep(NA_character_, 3)))
  expect_error(
    base_colors(coords, 1:5, limits = c(1, NA)),
    "finite numeric values"
  )
  expect_error(
    base_colors(coords, 1:5, limits = c(2, 1)),
    "in increasing order"
  )
  expect_error(base_colors(coords, 1:5, num_colors = 0), "positive integer")
})

# These resolver tests protect the shared specification's row selection and
# palette metadata, which renderer output alone cannot fully distinguish.
test_that("color resolution reverses palettes without changing identity colors", {
  labels <- factor(c("a", "a", "b"), levels = c("a", "b"))
  forward <- vizier:::resolve_colors(
    labels,
    NULL,
    n = 3,
    color_scheme = c("red", "blue")
  )
  reversed <- vizier:::resolve_colors(
    labels,
    NULL,
    n = 3,
    color_scheme = c("red", "blue"),
    rev = TRUE
  )
  identity <- vizier:::resolve_colors(
    c("red", "red", "blue"),
    NULL,
    n = 3,
    rev = TRUE
  )

  expect_identical(forward$colors, c("red", "red", "blue"))
  expect_identical(reversed$colors, c("blue", "blue", "red"))
  expect_identical(identity$colors, c("red", "red", "blue"))
})

test_that("numeric resolution uses and reverses the Viridis default", {
  forward <- vizier:::resolve_colors(c(1, 2, 3), NULL, n = 3, num_colors = 3)
  reversed <- vizier:::resolve_colors(
    c(1, 2, 3),
    NULL,
    n = 3,
    num_colors = 3,
    rev = TRUE
  )

  expect_identical(
    forward$palette,
    grDevices::hcl.colors(3, palette = "Viridis")
  )
  expect_identical(reversed$palette, rev(forward$palette))
  expect_false(identical(forward$palette, vizier:::make_palette(3)))
})

test_that("row defaults are stable Polychrome prefixes then Dynamic HCL", {
  row36 <- vizier:::resolve_colors(NULL, NULL, n = 36)
  row37 <- vizier:::resolve_colors(NULL, NULL, n = 37)

  expect_identical(
    row36$colors,
    unname(grDevices::palette.colors(36, palette = "Polychrome 36"))
  )
  expect_identical(
    row37$colors,
    grDevices::hcl.colors(37, palette = "Dynamic")
  )
})

test_that("direct character vectors are categorical while data-frame inference is conservative", {
  one_level <- vizier:::resolve_colors(rep("group", 2), NULL, n = 2)
  unique_values <- vizier:::resolve_colors(c("a", "b"), NULL, n = 2)
  inferred <- vizier:::resolve_colors(
    data.frame(id = c("a", "b")),
    NULL,
    n = 2
  )

  expect_identical(one_level$kind, "discrete")
  expect_identical(unique_values$kind, "discrete")
  expect_identical(inferred$kind, "row")
})

test_that("named categorical palettes map by category name", {
  palette <- c(a = "red", b = "blue", unused = "green")
  labels <- factor(c("b", "a", "b"), levels = c("b", "a"))
  resolved <- vizier:::resolve_colors(
    labels,
    NULL,
    n = 3,
    color_scheme = palette
  )
  reversed <- vizier:::resolve_colors(
    labels,
    NULL,
    n = 3,
    color_scheme = palette,
    rev = TRUE
  )

  expect_identical(resolved$colors, c("blue", "red", "blue"))
  expect_identical(reversed$colors, c("red", "blue", "red"))
  expect_error(
    vizier:::resolve_colors(labels, NULL, n = 3, color_scheme = c(a = "red")),
    "missing observed categories"
  )
  expect_error(
    vizier:::resolve_colors(
      labels,
      NULL,
      n = 3,
      color_scheme = c(a = "red", "blue")
    ),
    "complete, unique, and non-empty"
  )
})

test_that("numeric top selection is exact, stable, and distinct from missing values", {
  resolved <- vizier:::resolve_colors(
    c(10, 10, 5, NA),
    NULL,
    n = 4,
    color_scheme = c("black", "white"),
    num_colors = 2,
    top = 1,
    NA_color = "grey"
  )

  expect_identical(resolved$keep, c(TRUE, FALSE, FALSE, FALSE))
  expect_false(is.na(resolved$colors[[1]]))
  expect_true(all(is.na(resolved$colors[-1])))
  expect_error(
    vizier:::resolve_colors(c(1, 2), NULL, n = 2, top = 3),
    "positive integer"
  )
  expect_error(
    vizier:::resolve_colors(c("a", "b"), NULL, n = 2, top = 1),
    "only supported"
  )
})

test_that("resolve_colors returns renderer-neutral specifications", {
  specs <- list(
    identity = vizier:::resolve_colors(NULL, "#AA0000", n = 2),
    discrete = vizier:::resolve_colors(
      factor(c("a", "b")),
      NULL,
      n = 2,
      color_scheme = c("red", "blue")
    ),
    continuous = vizier:::resolve_colors(
      c(3, 1, NA),
      NULL,
      n = 3,
      color_scheme = c("black", "white"),
      top = 1
    ),
    row = vizier:::resolve_colors(NULL, NULL, n = 2)
  )

  expect_identical(
    unname(vapply(specs, `[[`, character(1), "kind")),
    names(specs)
  )
  expect_identical(specs$identity$keep, c(TRUE, TRUE))
  expect_identical(specs$discrete$palette, c(a = "red", b = "blue"))
  expect_identical(specs$continuous$keep, c(TRUE, FALSE, FALSE))
  expect_identical(specs$continuous$missing, c(FALSE, FALSE, TRUE))
  expect_length(specs$row$colors, 2)
})
