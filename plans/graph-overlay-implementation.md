# Implementation plan for graph overlays in vizier

Milestone 1 is implemented in the working tree. The three plotters accept straight overlays,
and the [iris gallery](../vignettes/articles/graph-overlays.Rmd) covers examples 1–3. Its third
example uses Plotly so readers can inspect each flower's score. The site renders from a saved iris
fit and score vector. Milestone 2, point inspection by click, is next. Weighted Hammer routing
remains milestone 3.

The full package check, including the PDF manual, passed with 472 test assertions passing in the
local suite. A browser check found 97 separate edge paths, 150 markers, a numeric colorbar and
the correct row, species and score on hover. The gallery reproduced 5, 11 and 97 distinct
cross-species pairs for the nearest, furthest-retained and fitted views.

The [feasibility assessment](graph-overlays.md) records the findings on vizier 0.6.2. The
[iris probe](graph-overlay-probe.R) reproduces the endpoint counts and static Plotly payloads.

## Public API

| Call | Contract |
| --- | --- |
| `embed_edges(x, select = "all", k = NULL)` | Return an ordinary edge data frame with one-based integer `from` and `to` in original observation order. Include `rank`, `distance` and `weight` when the input supplies them. |
| `embed_plot(coords, ..., overlays = NULL)` and its ggplot2 and Plotly counterparts | Append `overlays` after the existing arguments. Passing an edge table draws straight, uniform edges behind the points or text. A caller can supply their own table or subset the result of `embed_edges()`. |
| `prepare_overlay(coords, overlay, pc_axes = FALSE)` | In the Hammer extension, resolve a specification such as `list(edges = edges, route = "hammer", ...)` into a reusable, list-like object. All three plotters accept either the specification or the prepared object in `overlays`. |

The ellipses above stand for existing plot arguments. Preserve positional calls, return values and
point-only behavior when `overlays` is omitted.

### Edge selection

- **Ranked neighbors.** Accept a uwot metric block such as `fit$nn[[1]]`, a direct rnndescent
  `idx`/`dist` result, or `rnnd_build(...)$graph`. The index matrix has one row per observation.
  Accept integer-valued R indices, including doubles. Check matching dimensions and nondecreasing
  distances when `dist` is present. Allow `list(idx = ranked_indices)` for already-ranked indices.
  Both endpoints must index the same observation set.
- Remove every occurrence of the source row's own ID, keep the first occurrence of each other ID,
  and preserve supplied order within distance ties. A distinct observation at distance zero counts.
  Rank the survivors starting at one. uwot's requested 15 neighbors usually includes self.
- `"nearest"` takes the first survivor. `"furthest"` takes the last retained input-space neighbor
  per row. With `k = 10` it requests exactly the tenth non-self neighbor and reports a short row if
  that rank is unavailable. `"all"` returns every survivor, or up to the first `k` per row. Preserve
  directed choices in the result. Collapse reciprocal strokes when drawing a uniform overview.
- **Fitted graph.** Accept a square, symmetric numeric sparse Matrix. Drop stored zeros, take the
  strict upper triangle with `Matrix::triu(x, k = 1)`, then extract sparse triplets. A nonzero
  gives one undirected edge with its membership strength. Accept only `select = "all"` and
  `k = NULL` here. Symmetrization loses the original directional ranks.

## Milestone 1 — straight overlays and the first gallery release

1. Add `embed_edges()` and shared endpoint validation in `R/overlays.R`. Range-check against the
   full coordinate matrix, including isolated observations. Reuse `prepare_coords()` and
   `resolve_colors()`. Look up coordinates after `pc_axes` rotation. Where `top` is supported, draw
   an edge only when both original IDs are kept. A filtered row number must never become a new ID.
2. In `R/vizier.R`, use `panel.first` to draw vectorized `graphics::segments()` behind the existing
   point or text pass. In `R/ggplot.R`, insert `geom_segment()` before the point/text layer with
   its own data, fixed colour, `inherit.aes = FALSE` and `show.legend = FALSE`.
3. In `R/plotly.R`, build the existing point widget first and prepend a single `scatter` line
   trace to the built trace list. Put `NA` breaks between pairs, set `connectgaps = FALSE`,
   `hoverinfo = "skip"` and `showlegend = FALSE`. The point traces retain their palettes, numeric
   colorbars, hover text and legend behavior. A categorical legend toggle controls points while
   the static overlay continues to show the chosen graph.
4. Add Matrix to `Suggests` for sparse input. Keep ordinary tables and ranked lists usable with
   base graphics. Document the exported helper and shared plot argument with roxygen, regenerate
   `man/` and `NAMESPACE`, and add a NEWS entry. uwot, rnndescent and quadra are example producers.

**Release checks.** Use a small hand-checkable fixture with a self ID outside column one, repeated
self, a zero-distance tie, reciprocal choices and an isolated vertex. Check the selected IDs, ranks
and distances, and the single weighted row per undirected sparse edge. Include an empty edge table.
In the existing renderer tests, compare point-only and overlay calls with filtered rows, rotated
coordinates, marker/text modes and explicit, discrete and numeric colors. Inspect ggplot layer order
and built Plotly traces, including JSON path breaks, hover text, legends and colorbars. Run the
package tests and `R CMD check` before release.

## Milestone 2 — inspect one observation in Plotly

Use straight edges for the first click-to-inspect mode. Give each point its original row ID in
`customdata` before Plotly splits categories into traces. On a point click, use that ID to retrieve
incoming and outgoing incident edges and replace the contents of one reusable line trace. Store a
compact adjacency index in a standalone widget. Panning, zooming and ordinary tooltips require no
graph lookup.

Exercise this in a browser with category-split points, a second click, and an isolated point.
Verify the displayed endpoints and that previous edges clear. Measure initial payload, load time
and click-to-draw time on a larger graph. The iris probe's 23 kB figure contained one displayed
neighborhood. It did not include a complete interactive adjacency index or measure browser time.

## Milestone 3 — prepare weighted Hammer paths once

1. Add `prepare_overlay()` and optional igraph and edgebundle dependencies. Normalize the selected
   edge table, retain a stable edge ID, and create vertices in coordinate-row order, including
   isolates. Run `edge_bundle_hammer()` once in the resolved coordinate system. Associate each
   path group with its edge ID through igraph edge order. Keep ordered path vertices, endpoint IDs
   and the original edge weights in the prepared result.
2. Passing an inline Hammer specification prepares it for that call. Passing a prepared object
   uses its stored geometry. Check compatibility with the plotted coordinate rows and `pc_axes`
   setting. Reprepare for another embedding or to route a different edge set. A `top` display mask
   may hide complete paths by endpoint ID. To bundle just the visible subgraph, subset the edge
   table before preparation.
3. Resolve edge style at draw time. `alpha = "weight"` maps `alpha_limits` linearly to
   `alpha_range`. With the gallery settings this is `0.01 + 0.24 * weight`. Leave `alpha_bins`
   unset for continuous opacity. An explicit `alpha_bins = 4L` rounds to the nearest of four
   equally spaced opacity levels, including both endpoints. Changing style leaves the paths intact.
4. Base draws `lines()` with a constant stroke per path or batch of like-styled paths. ggplot2
   draws `geom_path()` grouped by edge ID beneath the points, using the resolved edge opacity
   independently of the point scales. Plotly batches paths by opacity with the same breaks and
   hover settings as straight edges. Exact opacity can require one trace per distinct weight.
   Four bins require at most four traces and still transmit every sampled path vertex.

**Release checks.** Match each path's first and last coordinates and weight to the originating
edge, allowing reversal for an undirected pair. Count routing calls across preparation and the
three gallery calls below. The expected count is one. Verify that
base and ggplot2 receive continuous opacities, Plotly emits at most four edge traces, and binning
preserves every ordered path. Check the point traces and legends against the straight version.
The probe measured 139 kB straight and 2.65 MB bundled for the same one-style iris graph. Its
1,208 distinct memberships explain the value of explicit binning.

## Iris gallery — Which groups are connected in an embedding?

Add `vignettes/articles/graph-overlays.Rmd` to the Guides in `_pkgdown.yml`. Generate the figures
from the same fit and keep the examples in reading order. Publish examples 1–3 with milestone 1
and add example 4 with milestone 3. The fourth example targets the planned Hammer API.

```r
library(vizier)

X <- as.matrix(iris[, 1:4])
species <- iris$Species
set.seed(65)
fit <- uwot::umap(
  X, n_neighbors = 15, n_threads = 1, ret_nn = TRUE, ret_extra = "fgraph"
)
xy <- fit$embedding
edges <- embed_edges(fit$fgraph)
between <- subset(edges, species[from] != species[to])
```

### 1. Which species share a fitted edge?

```r
embed_ggplot(xy, x = species, fixed_aspect = TRUE)
embed_ggplot(xy, x = species, overlays = between, fixed_aspect = TRUE)
with(between, table(species[from], species[to]))
```

Show the two maps side by side with identical axes and palette. In the checked fit, 97 unique
edges joined versicolor to virginica, and none joined setosa to another species. Count the
endpoints for the displayed fit so the caption stays tied to the figure.

### 2. Which bridges survive a one-neighbor view?

```r
nn <- fit$nn[[1]]
nearest <- embed_edges(nn, select = "nearest")
furthest <- embed_edges(nn, select = "furthest")
tenth <- embed_edges(nn, select = "furthest", k = 10)

embed_plot(xy, x = species, overlays = nearest, fixed_aspect = TRUE)
embed_plot(xy, x = species, overlays = furthest, fixed_aspect = TRUE)
embed_plot(xy, x = species, overlays = edges, fixed_aspect = TRUE)
```

Caption these panels with the number of distinct cross-species endpoint pairs. After selection,
count each unordered pair once using `pmin(from, to)` and `pmax(from, to)`. The reference counts
were 5, 11 and 97. A missing line in the first two panels cannot rule out a fitted connection.

### 3. How well are the endpoints' neighborhoods preserved?

```r
quality <- quadra::nn_preservation(
  X, xy, k = 10, nn_method_in = "brute", metric_in = "euclidean",
  metric_out = "euclidean", ret_extra = TRUE
)
retained <- quality$nnpv$nnp10
embed_ggplot(
  xy, x = retained, overlays = between, limits = c(0, 1),
  color_scheme = "grDevices::Viridis", fixed_aspect = TRUE
) + ggplot2::labs(colour = "Fraction of 10 neighbors retained")
```

Keep the cross-species edges from example 1. A point at 0.7 kept seven of its ten input-space
neighbors in 2D. Color describes a vertex and membership weight describes an edge.

### 4. What does bundling change?

```r
weighted_hammer <- list(
  edges = edges, route = "hammer", alpha = "weight",
  alpha_limits = c(0, 1), alpha_range = c(0.01, 0.25)
)
hammer <- prepare_overlay(xy, weighted_hammer)
hammer_plotly <- hammer
hammer_plotly$alpha_bins <- 4L

embed_plot(xy, x = retained, overlays = hammer, limits = c(0, 1), fixed_aspect = TRUE)
embed_ggplot(xy, x = retained, overlays = hammer, limits = c(0, 1), fixed_aspect = TRUE)
embed_plotly(xy, x = retained, overlays = hammer_plotly, limits = c(0, 1), fixed_aspect = TRUE)
```

Show a straight reference with the same full edge set and point colors alongside the bundle.
The base and ggplot2 calls use continuous opacity. Plotly uses 0.01, 0.09, 0.17 and 0.25.
All three draw the same prepared paths. A curve passing near a third observation supplies no
evidence of a connection to it.

Before publishing each gallery increment, execute its displayed code from a fresh session with the
example packages installed, render the article, and inspect the figures and widget. Derive captions
from that run. The reviewed iris counts are reference observations rather than test fixtures.
