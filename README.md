# vizier

<!-- badges: start -->
[![R-CMD-check](https://github.com/jlmelville/vizier/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jlmelville/vizier/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/jlmelville/vizier/graph/badge.svg?token=7QJD0e9arV)](https://codecov.io/github/jlmelville/vizier)
[![Last Commit](https://img.shields.io/github/last-commit/jlmelville/vizier)](https://github.com/jlmelville/vizier)
<!-- badges: end -->

An R Package for Visualization of 2D Datasets.

Visualizing datasets in 2D (e.g. via PCA, Sammon Mapping, t-SNE) is much more informative if the
points are colored, using something like:

* Factor levels mapped to different colors.
* A numeric value mapped to a color scale.
* A string encoding a color.

This package is to make doing that a bit easier, using the `graphics::plot` function, a returned
[ggplot2](https://ggplot2.tidyverse.org/) object, or the [plotly](https://plotly.com/r/) JavaScript
library. If you don't specify a specific column to color by, it will attempt to find a suitable
factor or color column automatically, using the last suitable column found, so you can add a custom
column to a dataframe if needed and have it picked out automatically.

## Installing

```R
install.packages("pak")
pak::pak("jlmelville/vizier")
```

## Documentation

```R
?embed_plot
?embed_ggplot
?embed_plotly
```

The pkgdown site has longer guides:

* [Getting started](https://jlmelville.github.io/vizier/articles/getting-started.html)
* [Color schemes](https://jlmelville.github.io/vizier/articles/color-schemes.html)

## Example

Create a plot of the first two principal components (PCA) for the `iris`
dataset:

```R
pca_iris <- stats::prcomp(iris[, -5], retx = TRUE, rank. = 2)
```

Simplest use of `embed_plot`: pass in data frame and it will use the last (in this case, only)
factor column it finds and the stable built-in `Polychrome 36` categorical palette.

```R
embed_plot(pca_iris$x, iris)
```
![Default embed plot result](man/figures/embed_ex.png "embed_plot(pca_iris$x, iris)")


For more examples, see the [Getting started](https://jlmelville.github.io/vizier/articles/getting-started.html)
article.

## License

[GPL (>= 3)](https://www.gnu.org/licenses/gpl-3.0.en.html). The code for the
`turbo` color scheme is from <https://gist.github.com/jlmelville/be981e2f36485d8ef9616aef60fd52ab>
and is licensed under Apache 2.

## See Also

* More example datasets that I've used these functions with can be found in the
[snedata](https://github.com/jlmelville/snedata) package.
* [quadra](https://github.com/jlmelville/quadra) for assessing the results quantitatively.
