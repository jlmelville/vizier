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
