# vizier

An R Package for Visualization of 2D Datasets.

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

If you install the [RColorBrewer](https://cran.r-project.org/package=RColorBrewer)
you can also use [ColorBrewer](http://www.colorbrewer2.org) color schemes.

```R
library("RColorBrewer")
embed_plot(pca_iris$x, iris, color_scheme = "Dark2")
```
![Embed plot with ColorBrewer color scheme](img/embed_ex_cb.png "embed_plot(pca_iris$x, iris, color_scheme = \"Dark2\")")


```R
# Visualize numeric value (petal length) as a color (the "Blues" scheme also
# needs RColorBrewer)
embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "Blues")
```
![Embed plot with quantitative color scale](img/embed_ex_quant.png "embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = \"Blues\")")

```R
# Just show the points with the 10 longest petals
embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = "Blues", top = 10)
```
![Embed plot only showing top 10 petal lengths](img/embed_ex_top.png "embed_plot(pca_iris$x, iris$Petal.Length, color_scheme = \"Blues\", top = 10)")

If you install the [plotly](https://cran.r-project.org/package=plotly) package,
you can use the `embed_plotly` function which has the same interface as 
`embed_plot` (except the `top` parameter is missing). This has the advantage
of showing a legend and tooltips:

```R
library(plotly)
embed_plotly(pca_iris$x, iris)
```
![Embed plot as a webpage with plotly](img/embed_ex_plotly.png "embed_plotly(pca_iris$x, iris)")

```R
# Don't have to see a legend if custom tooltips will do
embed_plotly(pca_iris$x, iris, show_legend = FALSE, tooltip = paste("Species:", iris$Species))
```

![plotly with custom tooltips](img/embed_ex_plotly_tooltip.png "embed_plotly(pca_iris$x, iris, show_legend = FALSE, tooltip = paste(\"Species:\", iris$Species))")

## License

[MIT](https://opensource.org/licenses/MIT).

## See Also
* More example datasets that I've used these functions with can be found in the [snedata](https://github.com/jlmelville/snedata) and [COIL-20](https://github.com/jlmelville/coil20) packages.
* [quadra](https://github.com/jlmelville/quadra) for assessing the results quantitatively. This one's a bit rough at the moment, though.
