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
