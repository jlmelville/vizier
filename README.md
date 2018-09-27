# vizier

An R Package for Visualization of 2D Datasets.

**September 27 2018**: Color schemes with `embed_plotly` was badly messed up.
This now fixed. You now also have control over whether to interpolate a discrete
palette.

Visualizing datasets in 2D (e.g. via PCA, Sammon Mapping, t-SNE) is much more
informative if the points are colored, using something like:

* Factor levels mapped to different colors.
* A numeric value mapped to a color scale.
* A string encoding a color.

This package is to make doing that a bit easier, using the `graphics::plot`
function, or via the [plotly](https://plot.ly/) JavaScript library. If you
don't specify a specific column to color by, it will attept to find a suitable
factor or color column automatically, using the last suitable column found, so
you can add a custom column to a dataframe if needed and have it picked out 
automatically.

## Installing

```R
install.packages("devtools")
devtools::install_github("jlmelville/vizier")
```

## Documentation

```R
?embed_plot
?embed_plotly
```

## Examples

```R
# Embed with PCA
pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)
```

```R
# Simplest use of embed_plot: pass in data frame and it will use the last 
# (in this case, only) factor column it finds and the rainbow color scheme
embed_plot(pca_iris$x, iris)
```
![Default embed plot result](img/embed_ex.png "embed_plot(pca_iris$x, iris)")


```R
# More explicitly color by iris species, use the rainbow color scheme and also
# provide a title and subtitle
embed_plot(pca_iris$x, iris$Species, color_scheme = rainbow, title = "iris PCA", sub = "rainbow color scheme")
```
![Embed plot with a title](img/embed_ex_title.png "embed_plot(pca_iris$x, iris$Species, color_scheme = rainbow, title = \"iris PCA\")")

```R
# Increase the transparency of the fill color by scaling the alpha by 0.5
embed_plot(pca_iris$x, iris$Species, color_scheme = rainbow, alpha_scale = 0.5)
```
![Embed plot with transparency](img/embed_ex_alpha.png "embed_plot(pca_iris$x, iris$Species, color_scheme = rainbow, alpha_scale = 0.5)")

```R
# topo.colors scheme
embed_plot(pca_iris$x, iris$Species, color_scheme = topo.colors)
```

![Embed plot with a topo color scheme](img/embed_ex_topo.png "embed_plot(pca_iris$x, iris$Species, color_scheme = topo.colors)")


```R
# Force axes to be equal size to stop cluster being distorted in one direction
embed_plot(pca_iris$x, iris$Species, color_scheme = topo.colors, equal_axes = TRUE)
```

![Embed plot with equal axes](img/embed_ex_ax.png "embed_plot(pca_iris$x, iris$Species, color_scheme = topo.colors, equal_axes = TRUE)")

```R
# Can plot the category names instead of points, but looks bad if they're
# long (or the dataset is large. Make the text a bit smaller with the cex param
embed_plot(pca_iris$x, iris$Species, cex = 0.75, text = iris$Species)
```

![Embed plot with text labels](img/embed_ex_text.png "embed_plot(pca_iris$x, iris$Species, cex = 0.75, text = iris$Species)")

For more color schemes, Vizier makes use of the excellent 
[paletteer](https://cran.r-project.org/package=paletteer) package. 
You can select one of the palettes on offer by (among other ways) passing a 
string with the format`"package::palette"`. For example, to use the `Dark2`
scheme from the the 
[RColorBrewer](https://cran.r-project.org/package=RColorBrewer) package 
(itself based on [ColorBrewer](http://www.colorbrewer2.org) schemes):

```R
embed_plot(pca_iris$x, iris, color_scheme = "RColorBrewer::Dark2")
```
![Embed plot with ColorBrewer color scheme](img/embed_ex_cb.png "embed_plot(pca_iris$x, iris, color_scheme = \"RColorBrewer::Dark2\")")

For more on selecting color schemes, see the 'Color Schemes' section below.
Here's another example, using a continuous palette from RColorBrewer, useful
for mapping numeric vectors to the color:

```R
# Visualize numeric value (petal length) as a color
embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues")
```
![Embed plot with quantitative color scale](img/embed_ex_quant.png "embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = \"RColorBrewer::Blues\")")

```R
# Just show the points with the 10 longest petals
embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues", top = 10)
```
![Embed plot only showing top 10 petal lengths](img/embed_ex_top.png "embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = \"RColorBrewer::Blues\", top = 10)")

If you install the [plotly](https://cran.r-project.org/package=plotly) package,
you can use the `embed_plotly` function which has the same interface as 
`embed_plot` (except the `top` and `sub` parameters are missing). This has the advantage
of showing a legend and tooltips:

```R
embed_plotly(pca_iris$x, iris)
```
![Embed plot as a webpage with plotly](img/embed_ex_plotly.png "embed_plotly(pca_iris$x, iris)")

```R
# Don't have to see a legend if custom tooltips will do
embed_plotly(pca_iris$x, iris, show_legend = FALSE, tooltip = paste("Species:", iris$Species))
```

![plotly with custom tooltips](img/embed_ex_plotly_tooltip.png "embed_plotly(pca_iris$x, iris, show_legend = FALSE, tooltip = paste(\"Species:\", iris$Species))")

## Color Schemes

Vizier makes use of the wonderful 
[paletteer](https://cran.r-project.org/package=paletteer) package which unifies
the enormous number of palettes out there. To specify a color scheme, use the
`color_scheme` parameter, passing one of:

* A palette function that takes an integer `n` and returns a vector of colors,
e.g. `grDevices::rainbow`.
* A vector of colors making up a custom color scheme of your own devising, e.g.
`c('red', 'green', 'blue')`. There must be at least two colors in the list.
* The name of a color scheme provided by `paletteer`, in the form 
`"package::palette"`. For a list of the many, many palettes supported, see
[paletteer's github page](https://github.com/EmilHvitfeldt/paletteer). Some
examples include `"dutchmasters::milkmaid"`, `"cartography::green.pal"`, 
`"viridis::inferno"`, `"RColorBrewer::Dark2"`. `vizier` makes no distinction
between the continuous, fixed-width or dynamic palette classification used by
`paletteer`.

### Palette Interpolation

If the color scheme you select has a maximum number of colors, and `vizier`
needs to use more than those that are available, then it will interpolate
among the maximum number of colors to create the desired number. This may lead
to results where different categories are hard to distinguish from each other.
If you set `verbose = TRUE`, then if interpolation is required, a message will 
be logged to console to this effect. `paletteer` has information on the number
of colors available in each palette.

### Discrete Palettes with `continuous` Type

For discrete palettes, if you ask for fewer colors than the full range, you will
only get the first few colors from the palette. For some palettes this works 
fine. For example, here is the `Dark2` palette from `RColorBrewer`:

![RColorBrewer Dark2 swatch](img/dark2_swatch.png "swatches::show_palette(paletteer::paletteer_d(\"RColorBrewer\", \"Dark2\"))")

If you use this palette to color the iris PCA:

![iris PCA with Dark2 color scheme](img/embed_dark2.png "embed_plot(pca_iris$x, iris, color_scheme = \"RColorBrewer::Dark2\", cex = 2, title = \"RColorBrewer Dark2\")")

The three colors from the lefthand side of the swatch are used to color the 
species.

However, some discrete palettes have an ordering to them, e.g. they go to from
red to blue via yellow. Here's `rainbow` from the `jcolors` package:

![jcolors rainbow](img/rainbow_swatch.png "swatches::show_palette(paletteer::paletteer_d(\"jcolors\", \"rainbow\"))")

The PCA embedding now looks like:

![iris PCA with rainbow color scheme](img/embed_jcrainbow.png "embed_plot(pca_iris$x, iris, color_scheme = \"jcolors::rainbow\", cex = 2, title = \"jcolors rainbow\")")

If you would prefer to use a fuller extent of the palette, you can treat the
palette as continuous, by appending `::c` to the name of the color scheme, 
e.g. `"jcolors::rainbow::c"`. Now the result is:

![iris PCA with continuous rainbow color scheme](img/embed_jcrainbowc.png "embed_plot(pca_iris$x, iris, color_scheme = \"jcolors::rainbow::c\", cex = 2, title = \"jcolors rainbow (continuous)\")")

where the colors come from the left-most, right-most and center positions on
the swatch.

The downside to treating these palettes as continuous is that there is no 
guarantee that the interpolation will result in colors that actually come from
the palette. In fact, they probably won't. We just got lucky in the above
example, because interpolating between colors was not required. For colors which
show a natural progression like `jcolors::rainbow`, results should still be ok.
However, for palettes like `RColorBrewer::Dark2`, interpolation may not turn out
so well. The iris PCA with the "continuous" version of Dark2, i.e. specifying 
`RColorBrewer::Dark2::c` results in:

![iris PCA with continuous Dark2 color scheme](img/embed_dark2c.png "embed_plot(pca_iris$x, iris, color_scheme = \"RColorBrewer::Dark2::c\", cex = 2, title = \"RColorBrewer Dark2 (continuous)\")")

The left cluster uses the green from the left-hand of the Dark2 swatch, and the
right cluster is colored in the gray color from the right-hand side. But the
middle cluster isn't any of the other colors and mixes rather murkily with the
gray cluster. It doesn't make sense to use interpolation in this case.

In summary, avoid interpolation of discrete color schemes if you can, but 
definitely do so for those like `RColorBrewer::Dark2` which don't work on a
color scale.

## License

[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html).

## See Also

* More example datasets that I've used these functions with can be found in the [snedata](https://github.com/jlmelville/snedata) and [COIL-20](https://github.com/jlmelville/coil20) packages.
* [quadra](https://github.com/jlmelville/quadra) for assessing the results quantitatively. This one's a bit rough at the moment, though.
