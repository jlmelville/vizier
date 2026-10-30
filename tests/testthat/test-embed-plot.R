with_pdf_device <- function(code) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(
    {
      grDevices::dev.off()
      unlink(path)
    },
    add = TRUE
  )

  force(code)
}

test_that("embed_plot handles representative inputs on a graphics device", {
  coords <- cbind(
    x = c(0, 1, 4, 6),
    y = c(1, 3, 2, 8)
  )
  labels <- factor(c("a", "b", "a", "b"))

  with_pdf_device({
    expect_error(
      embed_plot(
        coords,
        labels,
        color_scheme = c("#AA0000", "#0055AA"),
        equal_axes = TRUE,
        title = "factor"
      ),
      NA
    )
    expect_error(
      embed_plot(
        list(coords = coords),
        c(10, 1, 5, 10),
        color_scheme = c("#000000", "#FFFFFF"),
        limits = c(1, 10),
        top = 2,
        NA_color = "grey",
        show_axes = FALSE,
        sub = "top values"
      ),
      NA
    )
    expect_error(
      embed_plot(
        coords,
        colors = c("#AA0000", "#0055AA", "#118833", "#882255"),
        text = letters[1:4],
        pc_axes = TRUE,
        rev = TRUE
      ),
      NA
    )
  })
})

test_that("embed_plot validates public inputs before opening a device", {
  coords <- cbind(1:2, 3:4)

  expect_error(embed_plot(matrix(1:3, ncol = 1)), "exactly two columns")
  expect_error(embed_plot(cbind(1, Inf)), "only finite values")
  expect_error(embed_plot(coords, x = 1:3), "one value per coordinate")
  expect_error(
    embed_plot(coords, colors = c("red", "blue", "green")),
    "length 1"
  )
  expect_error(embed_plot(coords, text = letters[1:3]), "length 1")
  expect_error(embed_plot(coords, alpha_scale = 2), "in \\[0, 1\\]")
  expect_error(embed_plot(coords, cex = 0), "finite positive")
  expect_error(
    embed_plot(coords, num_colors = 1.5, x = 1:2),
    "positive integer"
  )
  expect_error(
    embed_plot(coords, equal_axes = NA),
    "single non-missing logical"
  )
  expect_error(embed_plot(coords, fixed_aspect = NA), "'fixed_aspect'")
  expect_error(embed_plot(coords, fixed_aspect = 1), "'fixed_aspect'")
  expect_error(embed_plot(coords, xlim = c(1, NA)), "finite numeric")
  expect_error(
    embed_plot(matrix(c(1, 2), nrow = 1), pc_axes = TRUE),
    "at least two"
  )
})

test_that("one-row numeric x remains a continuous color input", {
  with_pdf_device({
    expect_error(
      embed_plot(matrix(c(1, 2), nrow = 1), x = 5),
      NA
    )
  })

  resolved <- vizier:::resolve_colors(5, NULL, n = 1)
  expect_identical(resolved$kind, "continuous")
})

test_that("embed_plot separates fixed aspect from coordinate limits", {
  coords <- cbind(c(0, 2, 4), c(10, 20, 30))
  regular <- base_plot_args(coords, colors = "red")
  fixed <- base_plot_args(
    coords,
    colors = "red",
    fixed_aspect = TRUE,
    xlim = c(-1, 5),
    ylim = c(9, 31)
  )
  automatic <- base_plot_args(coords, colors = "red", fixed_aspect = TRUE)

  expect_identical(regular$asp, NA)
  expect_identical(fixed$asp, 1)
  expect_equal(fixed$xlim, c(-1, 5))
  expect_equal(fixed$ylim, c(9, 31))
  expect_null(automatic$xlim)
  expect_null(automatic$ylim)

  for (fixed_aspect in c(FALSE, TRUE)) {
    equal <- base_plot_args(
      coords,
      colors = "red",
      equal_axes = TRUE,
      fixed_aspect = fixed_aspect,
      xlim = c(-1, 5),
      ylim = c(9, 31)
    )
    expect_identical(equal$asp, 1)
    expect_equal(equal$xlim, range(coords))
    expect_equal(equal$ylim, range(coords))
  }
})

test_that("embed_plot renders equal physical units for points and text", {
  coords <- cbind(c(0, 2, 4), c(10, 20, 30))
  with_pdf_device({
    for (text in list(NULL, letters[1:3])) {
      embed_plot(
        coords,
        colors = "red",
        text = text,
        fixed_aspect = TRUE,
        xlim = c(-1, 5),
        ylim = c(9, 31)
      )
      usr <- graphics::par("usr")
      pin <- graphics::par("pin")
      expect_equal(diff(usr[1:2]) / pin[1], diff(usr[3:4]) / pin[2])
    }
  })
})

test_that("embed_plot accepts scalar and per-observation point symbols", {
  coords <- cbind(1:4, 5:8)
  expect_equal(base_plot_args(coords, colors = "red")$pch, rep(20, 4))
  expect_equal(base_plot_args(coords, colors = "red", pch = 17)$pch, rep(17, 4))
  expect_equal(base_plot_args(coords, colors = "red", pch = NA)$pch, rep(NA, 4))

  with_pdf_device({
    for (symbols in list(c(16, 17, NA, 15), c("a", "+", "", NA_character_))) {
      args <- base_plot_args(coords, colors = "red", pch = symbols)
      expect_identical(args$pch, symbols)
      expect_no_warning(embed_plot(coords, colors = "red", pch = symbols))
    }
  })
})

test_that("embed_plot preserves symbol rows with top selection and missing colors", {
  coords <- cbind(1:5, 6:10)
  symbols <- c(16, NA, 17, 15, 18)
  selected <- base_plot_args(
    coords,
    c(9, NA, 9, 3, 1),
    top = 2,
    pch = symbols,
    NA_color = "grey70"
  )
  expect_identical(selected$coords, coords)
  expect_identical(selected$pch, symbols)
  expect_equal(
    unname(grDevices::col2rgb(selected$col, alpha = TRUE)[4, ]),
    c(255, 0, 255, 0, 0)
  )

  colors <- c("red", NA, "blue", "red", "blue")
  for (NA_color in list(NULL, "grey70")) {
    args <- base_plot_args(
      coords,
      colors = colors,
      pch = symbols,
      NA_color = NA_color
    )
    expected <- colors
    if (!is.null(NA_color)) {
      expected[2] <- NA_color
    }
    expect_identical(args$coords, coords)
    expect_identical(args$pch, symbols)
    expect_identical(args$col, grDevices::adjustcolor(expected))
  }
})

test_that("embed_plot ignores pch in text mode", {
  coords <- cbind(1:3, 4:6)
  captured <- NULL
  testthat::local_mocked_bindings(
    text = function(x, labels, ...) {
      captured <<- list(coords = x, labels = labels)
    },
    .package = "graphics"
  )
  args <- base_plot_args(
    coords,
    colors = "red",
    text = letters[1:3],
    pch = list("ignored"),
    fixed_aspect = TRUE
  )
  expect_identical(args$type, "n")
  expect_false("pch" %in% names(args))
  expect_identical(args$asp, 1)
  expect_identical(captured$coords, coords)
  expect_identical(captured$labels, letters[1:3])
})

test_that("embed_plot rejects invalid symbols before plotting", {
  coords <- cbind(1:4, 5:8)
  for (symbols in list(
    NULL,
    1:3,
    "long",
    Inf,
    1.5,
    .Machine$integer.max + 1,
    TRUE,
    list(20)
  )) {
    expect_error(embed_plot(coords, pch = symbols), "'pch'")
  }
})
