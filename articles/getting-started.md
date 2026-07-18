# Getting Started

## Examples

Create a plot of the first two principal components (PCA) for the `iris`
dataset:

``` r

pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)
```

Simplest use of embed_plot: pass in data frame and it will use the last
(in this case, only) factor column it finds and the stable built-in
`Polychrome 36` categorical palette. Numeric vectors instead use a
sequential HCL Viridis palette by default. If the built-in categorical
palette is not available, Vizier uses an HCL Dynamic fallback.

``` r

embed_plot(pca_iris$x, iris)
```

![Default embed plot
result](img/embed_ex.png "embed_plot(pca_iris$x, iris)")

Default embed plot result

More explicitly color by iris species, use the rainbow color scheme and
also provide a title and subtitle:

``` r

embed_plot(pca_iris$x, iris$Species, color_scheme = rainbow, title = "iris PCA", sub = "rainbow color scheme")
```

![Embed plot with a
title](img/embed_ex_title.png "embed_plot(pca_iris$x, iris$Species, color_scheme = rainbow, title = "iris PCA")")

Embed plot with a title

Increase the transparency of the fill color by scaling the alpha by 0.5:

``` r

embed_plot(pca_iris$x, iris$Species, color_scheme = rainbow, alpha_scale = 0.5)
```

![Embed plot with
transparency](img/embed_ex_alpha.png "embed_plot(pca_iris$x, iris$Species, color_scheme = rainbow, alpha_scale = 0.5)")

Embed plot with transparency

If you already have colors you want to use for each point, you can use
the `colors` parameter. In the example below,
`colorRampPalette(c("red", "yellow"))(nrow(iris))` produces a vector of
150 colors going from red to yellow:

``` r

my_iris_colors = colorRampPalette(c("red", "yellow"))(nrow(iris))
embed_plot(pca_iris$x, iris$Species, colors = my_iris_colors)
```

![Embed plot with
colors](img/embed_ex_colors.png "embed_plot(pca_iris$x, iris$Species, colors = my_iris_colors)")

Embed plot with colors

If you just want the points to be all one color you need only pass a
single value, e.g.  `colors = "blue"`. In general, if you pass fewer
colors than there are points, the colors are recycled.

Here’s another example of using a built-in palette, `topo.colors`:

![Embed plot with a topo color
scheme](img/embed_ex_topo.png "embed_plot(pca_iris$x, iris$Species, color_scheme = topo.colors)")

Embed plot with a topo color scheme

This package also includes the [turbo
colormap](https://ai.googleblog.com/2019/08/turbo-improved-rainbow-colormap-for.html)
as a palette, via the `turbo` function, which works a lot like
[`grDevices::rainbow`](https://rdrr.io/r/grDevices/palettes.html)
(although reversed in terms of colors):

``` r

embed_plot(pca_iris$x, iris$Species, color_scheme = turbo)
```

![Embed plot with the turbo color
scheme](img/embed_ex_turbo.png "embed_plot(pca_iris$x, iris$Species, color_scheme = turbo)")

Embed plot with the turbo color scheme

The `rev` argument reverses a generated color scheme before it is mapped
to categories or numeric values. It does not reorder explicitly supplied
per-row colors:

``` r

embed_plot(pca_iris$x, iris$Species, color_scheme = turbo, rev = TRUE)
```

![Embed plot with the turbo color scheme
reversed](img/embed_ex_turbo_rev.png "embed_plot(pca_iris$x, iris$Species, color_scheme = turbo, rev = TRUE)")

Embed plot with the turbo color scheme reversed

You can also provide your own palette (i.e. a vector of colors):

``` r

embed_plot(pca_iris$x, iris$Species, color_scheme = c("black", "red", "gray"))
```

![Embed plot with custom
palette](img/embed_ex_custom.png "embed_plot(pca_iris$x, iris$Species, color_scheme = c("black", "red", "gray"))")

Embed plot with custom palette

Note that if you have more colors in your palette than needed, the extra
ones are ignored: e.g. if `c("black", "red", "gray", "blue")`, `"blue"`
would have been unused, because we only needed three colors from the
palette for this plot for the three species.

A fully named palette maps categories by name, so it remains stable
after reordering or subsetting. Extra names are ignored; every observed
category must have a named color:

``` r

species_colors <- c(setosa = "#E69F00", versicolor = "#56B4E9", virginica = "#009E73")
embed_plot(pca_iris$x, iris$Species, color_scheme = species_colors)
```

Watch out for the opposite situation where you need *more* colors than
your palette provides. In this case `vizier` will use interpolation to
get the colors it needs. This might work out for some palettes that
represent a continuous color scale (like `rainbow`), but will give weird
and probably undesirable results for discrete palettes. For more
details, see the [Discrete Palettes with `continuous`
Type](https://jlmelville.github.io/vizier/articles/color-schemes.html#discrete-palettes-with-continuous-type)
section in the color schemes article.

As of R 4.0, there are some [new color
palettes](https://developer.r-project.org/Blog/public/2019/11/21/a-new-palette-for-r/index.html).
You can see the options available via
[`grDevices::palette.pals()`](https://rdrr.io/r/grDevices/palette.html)
and generate the palette using
[`grDevices::palette.colors`](https://rdrr.io/r/grDevices/palette.html).
Here’s an example using the `"Okabe-Ito"` palette:

``` r

if (exists("palette.colors", where = "package:grDevices")) {
  embed_plot(pca_iris$x, iris$Species, color_scheme = palette.colors(palette = "Okabe-Ito"))
}
```

![Embed plot with new built-in
palette](img/embed_ex_okabe_ito.png "embed_plot(pca_iris$x, iris$Species, color_scheme = palette.colors(palette = "Okabe-Ito"))")

Embed plot with new built-in palette

For any palette in `palette.pals`, you can also just provide the palette
name as a shortcut:

    embed_plot(pca_iris$x, iris$Species, color_scheme = "Okabe-Ito")

To use common limits and equal physical X/Y units, preventing clusters
from being stretched on a non-square device:

``` r

embed_plot(pca_iris$x, iris$Species, color_scheme = topo.colors, equal_axes = TRUE)
```

![Embed plot with equal
axes](img/embed_ex_ax.png "embed_plot(pca_iris$x, iris$Species, color_scheme = topo.colors, equal_axes = TRUE)")

Embed plot with equal axes

You can plot the category names instead of points, but it looks bad if
they’re long (or the dataset is large). Making the text a bit smaller
with the `cex` param can help:

``` r

embed_plot(pca_iris$x, iris$Species, cex = 0.75, text = iris$Species)
```

![Embed plot with text
labels](img/embed_ex_text.png "embed_plot(pca_iris$x, iris$Species, cex = 0.75, text = iris$Species)")

Embed plot with text labels

For more color schemes, Vizier makes use of the excellent
[paletteer](https://cran.r-project.org/package=paletteer) package. You
can select one of the palettes on offer by (among other ways) passing a
string with the format `"package::palette"`. For example, to use the
`Dark2` scheme from the
[RColorBrewer](https://cran.r-project.org/package=RColorBrewer) package
(itself based on [ColorBrewer](http://www.colorbrewer2.org) schemes):

``` r

embed_plot(pca_iris$x, iris, color_scheme = "RColorBrewer::Dark2")
```

![Embed plot with ColorBrewer color
scheme](img/embed_ex_cb.png "embed_plot(pca_iris$x, iris, color_scheme = "RColorBrewer::Dark2")")

Embed plot with ColorBrewer color scheme

For more on selecting color schemes, see the [Color
schemes](https://jlmelville.github.io/vizier/articles/color-schemes.md)
article. Here’s another example, using a continuous palette from
RColorBrewer, useful for mapping numeric vectors to the color:

``` r

# Visualize numeric value (petal length) as a color
embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues")
```

![Embed plot with quantitative color
scale](img/embed_ex_quant.png "embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues")")

Embed plot with quantitative color scale

``` r

# Just show the points with the 10 longest petals
embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues", top = 10)
```

![Embed plot only showing top 10 petal
lengths](img/embed_ex_top.png "embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues", top = 10)")

Embed plot only showing top 10 petal lengths

`top` selects exactly that many finite numeric values in decreasing
order; ties are resolved by their existing row order. Direct character
vectors are treated as categories unless every non-missing entry is a
literal color. Character columns discovered from a data frame remain
conservatively inferred only when they are factor-like, avoiding
accidental coloring by identifier columns.

## ggplot2 support

If you install the [ggplot2](https://ggplot2.tidyverse.org/) package,
[`embed_ggplot()`](https://jlmelville.github.io/vizier/reference/embed_ggplot.md)
returns an ordinary ggplot object with the same coordinate and color
handling as
[`embed_plot()`](https://jlmelville.github.io/vizier/reference/embed_plot.md).
This makes it easy to add ggplot2 layers, labels, and themes. For
example, we can add an ellipse for each species:

``` r

iris_ggplot <- embed_ggplot(
  pca_iris$x,
  iris$Species,
  cex = 2,
  title = "iris PCA"
)

iris_ggplot +
  ggplot2::stat_ellipse(level = 0.8, linewidth = 0.8) +
  ggplot2::labs(color = "Species", subtitle = "80% confidence ellipses") +
  ggplot2::theme_minimal(base_size = 12)
```

![ggplot2 embedding with confidence
ellipses](img/embed_ex_ggplot_ellipse.png "embed_ggplot(pca_iris$x, iris$Species) with stat_ellipse and theme_minimal")

ggplot2 embedding with confidence ellipses

Numeric inputs use a native continuous ggplot2 scale and colorbar. The
usual ggplot2 labeling and theme functions can be added while Vizier
continues to handle the colors and fixed coordinates:

``` r

embed_ggplot(
  pca_iris$x,
  iris$Petal.Length,
  cex = 2,
  equal_axes = TRUE,
  title = "iris petal length"
) +
  ggplot2::labs(color = "Petal length") +
  ggplot2::theme_minimal(base_size = 12)
```

![ggplot2 embedding with a continuous
colorbar](img/embed_ex_ggplot_numeric.png "embed_ggplot(pca_iris$x, iris$Petal.Length, equal_axes = TRUE) with theme_minimal")

ggplot2 embedding with a continuous colorbar

## Plotly support

If you install the [plotly](https://cran.r-project.org/package=plotly)
package, you can use the `embed_plotly` function which has the same
interface as `embed_plot` (except the `top` and `sub` parameters are
missing). This has the advantage of showing categorical legends or
numeric colorbars and tooltips. `text` works for numeric colors as well;
default hover text preserves original row numbers without showing
literal input colors:

``` r

embed_plotly(pca_iris$x, iris, color_scheme = rainbow)
```

![Embed plot as a webpage with
plotly](img/embed_ex_plotly.png "embed_plotly(pca_iris$x, iris, color_scheme = rainbow)")

Embed plot as a webpage with plotly

``` r

# Don't have to see a legend if custom tooltips will do
embed_plotly(pca_iris$x, iris, color_scheme = rainbow,show_legend = FALSE, tooltip = paste("Species:", iris$Species))
```

![plotly with custom
tooltips](img/embed_ex_plotly_tooltip.png "embed_plotly(pca_iris$x, iris, color_scheme = rainbow, show_legend = FALSE, tooltip = paste("Species:", iris$Species))")

plotly with custom tooltips
