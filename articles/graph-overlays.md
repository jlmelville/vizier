# Graph overlays on an iris UMAP

In the iris UMAP below, setosa forms a group of its own. Versicolor and
virginica run together. To see which flowers link those two species, we
can take the edges from uwot’s fitted graph and draw them over the
points. Click any static plot for a closer look.

### Fitting UMAP to iris

The four numeric columns of `iris` go into UMAP. The two extras worth
keeping here are the ranked neighbors (`ret_nn = TRUE`) and the fitted
fuzzy graph (`ret_extra = "fgraph"`). All the plots below use one saved
run of this code.

``` r

X <- as.matrix(iris[, 1:4])
set.seed(65)
fit <- uwot::umap(
  X,
  n_neighbors = 15,
  n_threads = 1,
  ret_nn = TRUE,
  ret_extra = "fgraph"
)
```

``` r

library(vizier)

xy <- fit$embedding
species <- iris$Species
palette <- c(setosa = "#0072B2", versicolor = "#D55E00", virginica = "#009E73")
edges <- embed_edges(fit$fgraph)
#> Loading required namespace: Matrix
between <- subset(edges, species[from] != species[to])
```

### Edges between species

The fitted graph is symmetric, so
[`embed_edges()`](https://jlmelville.github.io/vizier/reference/embed_edges.md)
takes its upper triangle. We get one row per pair, with the membership
strength in `weight`. The `from` and `to` columns are row numbers in
`iris`, so `species[from] != species[to]` picks out the edges between
different species. The iris rows happen to be sorted by species, which
is why the counts fall on one side of the table.

``` r

with(between, table(species[from], species[to]))
#>             
#>              setosa versicolor virginica
#>   setosa          0          0         0
#>   versicolor      0          0        97
#>   virginica       0          0         0
```

``` r

map <- function(overlay = NULL) {
  embed_ggplot(
    xy,
    species,
    overlays = overlay,
    cex = 0.8,
    color_scheme = palette,
    fixed_aspect = TRUE
  ) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")
}
map() + ggplot2::labs(title = "Species", colour = NULL)
map(between) + ggplot2::labs(title = "Cross-species edges", colour = NULL)
```

![The two panels show the same iris observations. Cross-species edges
join versicolor and
virginica.](graph-overlays_files/figure-html/bridges-1.svg)![The two
panels show the same iris observations. Cross-species edges join
versicolor and
virginica.](graph-overlays_files/figure-html/bridges-2.svg)

In this fit, all 97 cross-species edges join versicolor to virginica.
Setosa has no fitted edge to either species.

### Keeping one neighbor per flower

A quick way to thin out the plot is to keep one neighbor per flower.
`fit$nn[[1]]` lists the neighbors from the four original measurements in
order of distance. The first panel below uses the nearest neighbor in
that list and the second uses the furthest.
[`embed_edges()`](https://jlmelville.github.io/vizier/reference/embed_edges.md)
skips the flower itself by row ID, so a different flower at zero
distance still counts. Two flowers can choose each other, which is why
`count_bridges()` counts each pair only once.

``` r

nn <- fit$nn[[1]]
nearest <- embed_edges(nn, select = "nearest")
furthest <- embed_edges(nn, select = "furthest")
views <- list(Nearest = nearest, Furthest = furthest, Fitted = edges)

count_bridges <- function(e) {
  cross <- e[species[e$from] != species[e$to], , drop = FALSE]
  nrow(unique(data.frame(
    a = pmin(cross$from, cross$to),
    b = pmax(cross$from, cross$to)
  )))
}
counts <- vapply(views, count_bridges, integer(1))
```

``` r

par(mfrow = c(1, 3), mar = c(1, 1, 3, 1), lwd = 1.25)
for (name in names(views)) {
  embed_plot(
    xy,
    species,
    overlays = views[[name]],
    cex = 0.8,
    color_scheme = palette,
    fixed_aspect = TRUE,
    show_axes = FALSE,
    title = sprintf("%s\n%d cross-species pairs", name, counts[[name]])
  )
}
```

![Three iris maps show the nearest choice, the furthest retained choice,
and every fitted edge.](graph-overlays_files/figure-html/compare-1.svg)

Keeping the nearest neighbor gives 5 distinct cross-species pairs.
Keeping the furthest gives 11. Either way, we miss plenty of connections
in the fitted graph. To pick the tenth neighbor specifically, use
`embed_edges(nn, select = "furthest", k = 10)`.

### Neighbor preservation in 2D

[quadra](https://github.com/jlmelville/quadra) is an R package for
measuring how well an embedding preserves the structure of the original
data. Install it from GitHub with `pak::pak("jlmelville/quadra")` (run
`install.packages("pak")` first if you need pak).

Of a flower’s ten nearest neighbors in the original measurements, how
many are still among its ten nearest neighbors on the map?
`quadra::nn_preservation()` gives us the fraction retained for each
flower. A score of 0.7 means seven flowers appear in both lists.

``` r

quality <- quadra::nn_preservation(
  X,
  xy,
  k = 10,
  nn_method_in = "brute",
  metric_in = "euclidean",
  metric_out = "euclidean",
  ret_extra = TRUE
)
retained <- quality$nnpv$nnp10
```

Hover over a flower for its row number, species and score. Click it to
draw its edges, then click another flower to switch. You can see more
than ten lines on a click because the lines show every edge uwot fitted
to that flower. The color comes from quadra’s ten-neighbor calculation
above.

``` r

embed_plotly(
  xy,
  retained,
  overlays = list(edges = edges, mode = "incident"),
  limits = c(0, 1),
  color_scheme = "grDevices::Viridis",
  fixed_aspect = TRUE,
  tooltip = sprintf(
    "%s | %d of 10 neighbors retained",
    species,
    round(10 * retained)
  )
)
```

The mean fraction retained here is 0.71. If you’re only interested in
connections between species, change `edges = edges` to
`edges = between`. Clicking a flower with no cross-species edges then
clears the lines.

### Bundling the fitted graph

With all the edges drawn, the straight lines pile up around some of the
flowers. In the right panel below we’ll bundle the edges and give the
stronger ones more ink. Membership weight maps linearly to opacity, from
0.01 at weight 0 to 0.25 at weight 1.
[`prepare_overlay()`](https://jlmelville.github.io/vizier/reference/prepare_overlay.md)
uses igraph and edgebundle to compute the Hammer paths once.

``` r

weighted_hammer <- list(
  edges = edges,
  route = "hammer",
  alpha = "weight",
  alpha_limits = c(0, 1),
  alpha_range = c(0.01, 0.25)
)
hammer <- prepare_overlay(xy, weighted_hammer)
```

``` r

par(mfrow = c(1, 2), mar = c(1, 1, 3, 1), lwd = 1.25)
embed_plot(
  xy,
  retained,
  overlays = edges,
  limits = c(0, 1),
  color_scheme = "grDevices::Viridis",
  fixed_aspect = TRUE,
  show_axes = FALSE,
  cex = 0.8,
  title = "Straight edges"
)
embed_plot(
  xy,
  retained,
  overlays = hammer,
  limits = c(0, 1),
  color_scheme = "grDevices::Viridis",
  fixed_aspect = TRUE,
  show_axes = FALSE,
  cex = 0.8,
  title = "Weighted Hammer paths"
)
```

![Side-by-side iris maps compare uniform straight edges with
membership-weighted Hammer paths for the same fitted
pairs.](graph-overlays_files/figure-html/hammer-base-1.svg)

Both panels connect the same 1,347 pairs. In the right panel, nearby
paths bend together and weaker edges fade. A curve can pass close to a
third flower on its way between the two flowers it actually connects.

[`embed_ggplot()`](https://jlmelville.github.io/vizier/reference/embed_ggplot.md)
can use `hammer` as it stands, with the same paths and continuous edge
opacity. The colorbar shows the fraction of neighbors each flower kept.

``` r

embed_ggplot(
  xy,
  retained,
  overlays = hammer,
  cex = 0.8,
  limits = c(0, 1),
  color_scheme = "grDevices::Viridis",
  fixed_aspect = TRUE
) +
  ggplot2::theme_classic() +
  ggplot2::labs(colour = "Fraction retained")
```

![Bundled iris graph with a colorbar showing each flower's fraction of
ten retained
neighbors.](graph-overlays_files/figure-html/hammer-ggplot-1.svg)

Plotly uses one line trace per distinct opacity. With continuous
opacity, we’d need 1,208 traces for this graph. Set `alpha_bins = 4L` on
a copy of `hammer` and each path gets the nearest of four opacities
(0.01, 0.09, 0.17 and 0.25). Now Plotly can draw the graph in four
traces. It still gets every sampled point along each path, and we can
still hover over a flower to read its score.

``` r

hammer_plotly <- hammer
hammer_plotly$alpha_bins <- 4L
embed_plotly(
  xy,
  retained,
  overlays = hammer_plotly,
  limits = c(0, 1),
  color_scheme = "grDevices::Viridis",
  fixed_aspect = TRUE,
  tooltip = sprintf(
    "%s | %d of 10 neighbors retained",
    species,
    round(10 * retained)
  )
)
```
