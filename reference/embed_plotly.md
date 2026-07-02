# Embedding Plot Using the JavaScript Library Plotly

Plots the embedded coordinates, with each point colored by a specified
color, using Plotly.

## Usage

``` r
embed_plotly(
  coords,
  x = NULL,
  colors = NULL,
  color_scheme = NULL,
  num_colors = 15,
  alpha_scale = 1,
  limits = NULL,
  clip_limit_values = TRUE,
  title = NULL,
  show_legend = TRUE,
  cex = 1,
  text = NULL,
  tooltip = NULL,
  equal_axes = FALSE,
  pc_axes = FALSE,
  xlim = NULL,
  ylim = NULL,
  rev = FALSE,
  verbose = FALSE
)
```

## Arguments

- coords:

  Matrix of embedded coordinates, with as many rows as observations, and
  2 columns.

- x:

  Either a data frame or a column that can be used to derive a suitable
  vector of colors. Ignored if `colors` is provided.

- colors:

  Vector containing colors for each coordinate.

- color_scheme:

  A color scheme. See 'Details'. Ignored if `colors` is specified.

- num_colors:

  Number of distinct colors to use in the palette, when `x` is a numeric
  vector, on the assumption that the palette is continuous (which it
  probably should be). Ignored if `x` is not a numeric vector. If set to
  `NULL`, it will be set to `length(x)`.

- alpha_scale:

  Scale the opacity alpha of the colors, between 0 and 1. Useful for
  increasing the transparency of points, especially with large plots
  with lots of overlap.

- limits:

  The range that the colors should map over when mapping from a numeric
  vector. If not specified, then the range of `x`. This is useful if
  there is some external absolute scale that should be used. Ignored if
  `x` is not a numeric vector.

- clip_limit_values:

  If `TRUE` (the default) and `limits` is provided, then any value that
  lies outside the `limits` is clipped to the limiting values. If
  `tooltip = NULL` then the original values will be preserved in the
  tooltip. If `clip_limit_values = FALSE`, then values outside the
  `limits` will be shown in the "missing" color.

- title:

  Title for the plot.

- show_legend:

  If `TRUE`, display a legend. Ignored unless a suitable categorical
  value is provided as `x` (or one can be found).

- cex:

  Size of the points. Ignored if `text` is provided.

- text:

  Vector of label text to display instead of a point. If the labels are
  long or the data set is large, this is unlikely to be very legible,
  but is occasionally useful.

- tooltip:

  Vector of tooltip text, to be displayed when a point is hovered over.

- equal_axes:

  If `TRUE`, the X and Y axes are set to have the same extents.

- pc_axes:

  If `TRUE`, the `coords` are replaced by the first two (unscaled)
  principal components, which should have the effect of rotating the
  data (with a potential reflection) so the main variance aligns along
  the X-axis. Should not have any other scaling effect.

- xlim:

  Vector two numeric value to give the numeric extent of the X-axis.
  Ignored if `equal_axes = TRUE` or `pc_axes = TRUE`.

- ylim:

  Vector two numeric value to give the numeric extent of the Y-axis.
  Ignored if `equal_axes = TRUE` or `pc_axes = TRUE`.

- rev:

  logical indicating whether the ordering of the colors should be
  reversed.

- verbose:

  If `TRUE`, log messages to the console, mainly when searching for a
  suitable color column in a dataframe.

## Details

This will open a web browser if you are using the R CLI. In RStudio, it
will put the plot in RStudio's Plots tab.

More information on plotly is available at its website,
<https://plotly.com/r/>.

The `x` argument can be used to provide a suitable vector of colors from
either a data frame or vector.

If a data frame is provided, then a vector of colors will be looked for.
If it's present, it will be used as the `colors` argument directly.
Otherwise, a factor column will be looked for, and each level will be
mapped to a different color. Otherwise, one color will be used for each
point. If more than one column of a type is found in the data frame, the
last one encountered is used.

If a vector is provided, a similar procedure to the data frame is used
when mapping from its content to a vector of colors. Additionally, a
numeric vector can be provided, which will be linearly mapped to a color
scheme.

The `color_scheme` parameter can be one of:

- A palette function that takes an integer `n` and returns a vector of
  colors, e.g.
  [`grDevices::rainbow`](https://rdrr.io/r/grDevices/palettes.html). For
  some other applicable functions, see the `Palettes` help page in the
  `grDevices` package (e.g. by running the
  [`?rainbow`](https://rdrr.io/r/grDevices/palettes.html) command).

- A vector of colors making up a custom color scheme of your own
  devising, e.g. `c('red', 'green', 'blue')`. There must be at least two
  colors in the list.

- The name of a color scheme provided by the
  [paletteer](https://cran.r-project.org/package=paletteer) package, in
  the form `"package::palette"`. Some examples include
  `"dutchmasters::milkmaid"`, `"cartography::green.pal"`,
  `"viridis::inferno"` and `"RColorBrewer::Dark2"`. If more colors are
  required than supported by the color scheme, interpolation will be
  used to create the required number of colors.

The default for a numeric scale is to use
[`rainbow`](https://rdrr.io/r/grDevices/palettes.html) and for factor
columns to use a palette generated by
[`createPalette`](https://rdrr.io/pkg/Polychrome/man/createPalette.html).

## Examples

``` r
if (FALSE) { # \dontrun{
# Embed with PCA
pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)

# Load the plotly package
library("plotly")

# Visualize the resulting embedding, colored by iris species, using the
# rainbow color scheme
embed_plotly(pca_iris$x, iris$Species,
  color_scheme = rainbow,
  title = "iris PCA"
)

# topo.colors scheme
embed_plotly(pca_iris$x, iris$Species, color_scheme = topo.colors)

# Pass in data frame and it will use the last (in this case, only) factor
# column it finds
embed_plotly(pca_iris$x, iris)

# Don't have to see a legend if the tooltips will do
embed_plotly(pca_iris$x, iris, show_legend = FALSE)

# Custom tooltips
embed_plotly(pca_iris$x, iris, tooltip = paste("Species:", iris$Species))

# Use the "Dark2" RColorBrewer scheme
embed_plotly(pca_iris$x, iris, color_scheme = "RColorBrewer::Dark2")

# Can plot the category names instead of points, but looks bad if they're
# long (or the dataset is large)
embed_plotly(pca_iris$x, iris$Species, cex = 0.5, text = iris$Species)

# Visualize numeric value (petal length) as a color
embed_plotly(pca_iris$x, iris$Petal.Length, color_scheme = "RColorBrewer::Blues")

# Can force axes to be equal size to stop cluster being distorted in one
# direction
embed_plotly(pca_iris$x, iris$Petal.Length,
  color_scheme = "RColorBrewer::Blues",
  equal_axes = TRUE
)
} # }
```
