# Graph overlays for vizier

Ship straight edges behind the points in all three plotters. Make the full fitted graph the example
for “do any observations in these two groups share an edge?” Offer nearest and furthest retained
neighbors as thinner views of ranked input. For large interactive plots, prioritize clicking a point
to see its incident edges. Keep Hammer as an opt-in overview for smaller graphs.

## Proposed API

```r
X <- as.matrix(iris[, 1:4])
fit <- uwot::umap(X, n_neighbors = 15, ret_nn = TRUE, ret_extra = "fgraph")
graph <- embed_edges(fit$fgraph)
near <- embed_edges(fit$nn[[1]], select = "nearest")
far <- embed_edges(fit$nn[[1]], select = "furthest")
tenth <- embed_edges(fit$nn[[1]], select = "furthest", k = 10)

embed_plot(fit$embedding, iris$Species, overlays = far)
embed_ggplot(fit$embedding, iris$Species, overlays = graph)
embed_plotly(fit$embedding, iris$Species, overlays = graph)

# Possible later extension, using exactly the same selected edges
embed_ggplot(fit$embedding, iris$Species, overlays = list(edges = graph, route = "hammer"))
```

`embed_edges(x, select = "all", k = NULL)` would return an ordinary data frame with one-based
`from` and `to`, plus the available `rank`, `distance` or `weight`. Append `overlays = NULL` to
each plotter. Passing this table, including a caller's own endpoint table, requests straight edges
with one subdued stroke. Callers can already subset the table or count cross-group pairs in R. One
helper and one argument cover the three renderers without multiplying plotting functions.

| Input to the helper | What one row in the result means |
| --- | --- |
| Ranked neighbors | A directed choice by observation `from`. Use the paired, same-shaped `n × k` matrices `idx` and `dist` in a uwot metric block (`fit$nn[[1]]` for a single metric), a direct rnndescent result such as `brute_force_knn()`, or `rnnd_build(...)$graph`. |
| Symmetric sparse adjacency | One undirected edge. For a square symmetric numeric `Matrix`, extract `Matrix::summary(Matrix::triu(Matrix::drop0(x), k = 1))` and use `i, j, x` as `from, to, weight`. In a uwot `fgraph` the value is membership strength. Only `select = "all"` with no rank cutoff applies. |

For ranked input, accept integer-valued indices even when stored as doubles. Both ends must refer
to the same observations in the same order. A query against a different reference set needs an
explicit row mapping first. Keep returned column order for distance ties, validate rank order when
distances are supplied, and also allow `list(idx = ranked_indices)` when the order is known. Drop
every occurrence of self by ID and keep the first occurrence of each other ID. A zero-distance
neighbor with a different ID is valid. Rank the remaining entries from one.

`nearest` selects rank one. `furthest` selects the last available in each row, or exactly rank
`k` when specified. Report a short row for an explicit kth request so it can't silently mean a
different rank. `all` returns every retained neighbor, optionally the first `k` per row. uwot's 15
usually includes self, yielding 14 other observations. On this iris run one uwot row contained
self twice and had only 13. With rnndescent, iris rows 102 and 143 tied at zero and row 143 put
itself second. Always deleting column one would select a self-loop there.
[uwot return values](https://jlmelville.github.io/uwot/reference/umap.html) ·
[rnndescent kNN result](https://jlmelville.github.io/rnndescent/reference/brute_force_knn.html) ·
[rnndescent index](https://jlmelville.github.io/rnndescent/reference/rnnd_build.html)

“Furthest” refers to input-space rank within that retained list. Global far points and edges with
the greatest 2D stretch would require different selections. Symmetrization and fuzzy weighting
lose directional ranks. Sorting membership strengths cannot recover them. The returned `fgraph`
can also omit weak edges according to the fitting epoch count. Use it directly for questions about
that fit. “Full” covers the graph's stored edges and needs only sparse extraction on input.
[uwot graph semantics](https://jlmelville.github.io/uwot/reference/umap.html)

## How the edges fit the current renderers

The checkout at `0b8c171` already centralizes coordinates in `prepare_coords()` and point styling
in `resolve_colors()`. Keep endpoint IDs in original row space. Check source dimensions against the
coordinate count when known, and validate table endpoints against `1:nrow(coords)`. Preserve
isolated rows. Resolve coordinates, including `pc_axes`, once. For `top` in base and ggplot2,
retain an edge only if both original IDs satisfy `color_res$keep`. Axis limits clip the drawing.
Point color and symbol choices remain styling decisions.

Select ranks before merging reciprocal pairs. Keep both directed records for inspection and draw
coincident strokes once in the uniform overview so opacity doesn't accidentally encode reciprocity.
Straight geometry needs only the two coordinate rows per pair. For Hammer, construct the graph
with all vertices in coordinate-row order, attach an edge ID, and map each path group through igraph
edge order to that ID. Carry `weight` along that lookup for styling. One ordered `edge_id, x, y`
path result in the prepared coordinate system serves every renderer.

| Renderer | Integration point |
| --- | --- |
| `embed_plot()` | Draw vectorized `graphics::segments()` in `panel.first` in the two current plot calls. For paths, use `lines()` with breaks between paths of the same style. The existing point or text pass then draws on top, including row-specific `pch`. |
| `embed_ggplot()` | Add `geom_segment()` or `geom_path(aes(group = edge_id))` **before** the existing point/text layer. Give edges their own data, `inherit.aes = FALSE`, fixed colour and `show.legend = FALSE`. A user-appended `p + geom_path(...)` paints over the points. |
| `embed_plotly()` | Build the existing point widget, then prepend ordinary `scatter` line specifications to its built `x$data`. Put `NA` between edges (JSON `null`), with `connectgaps = FALSE`, scalar `hoverinfo = "skip"` and `showlegend = FALSE`. Batch by stroke style. Keep the widget-specific list access in this adapter. |

I exercised both routes in base and ggplot2 on a temporary device with rotated coordinates, points
and text. With `top = 40`, 320 graph edges survived. The ggplot point/text layer data matched the
original. Plotly R 4.12.0 exposed a reason to preserve its existing widget construction. Starting
with a line trace and supplying the named palette only on the subsequent point trace silently
replaced categorical colors with library defaults. Prepending independent lines **after** building
the point widget preserved the point fields, hover, legend and axis settings across factor, numeric,
explicit and default colors, each with markers and text. Gaps serialized to `null`, and appending
another trace still worked. The categorical legend controls the point traces. The independent
static edge layer continues to show the selected graph when a category is hidden. These were
R/device and widget-build checks.

## What the examples establish

The 2023 example drew the last retained neighbor per observation. Iris was readable. Even that
sparse view of 70,000 MNIST observations was crowded and reportedly slow. A displayed pair is
evidence of that particular neighbor choice. Absence from this view cannot answer whether groups
share **any** fitted edge.
[Original experiment and discussion](https://github.com/jlmelville/uwot/issues/65#issuecomment-1436087688)

Counting original endpoints gives a checkable answer on the new iris fit. There were 97 distinct
versicolor–virginica pairs in the fitted graph and zero involving setosa and another species.
Nearest-only displayed five cross-species pairs and furthest-only eleven. Both used 150 directed
choices. All 2,099 retained non-self choices yielded 97 cross-species pairs here too. The equal
counts are specific to this fit. Counting endpoint labels answers direct adjacency without
interpreting projected gaps, curve crossings or bundle density as evidence about clusters.

The supplied iris example establishes that a graph from uwot 0.2.5 can be displayed over points
colored by a separate quadra score. The reported mean of 0.72 is the mean fraction of ten original
neighbors retained in 2D. The score says nothing by itself about *which* groups share graph edges.
The quadra MNIST article likewise uses point color to compare local preservation across maps. A
connectivity plot needs only coordinates and edges.
[Quadra worked comparison](https://jlmelville.github.io/quadra/articles/mnist-pca-umap.html)

In edgebundle 1.0.1, Hammer routes from endpoint coordinates. Both the supplied check and the local
probe matched all 1,347 paths to their endpoint rows and corresponding graph weights. Weights can
set opacity or width after routing. The default 50 samples per edge produced 67,350 path vertices.
A curve may pass an unrelated point, and a shared corridor can represent many different pairs.
Use straight incident edges to identify a point's neighbors.
[Hammer documentation](https://schochastics.github.io/edgebundle/reference/edge_bundle_hammer.html)

## Payload and release order

The [reproducible probe](graph-overlay-probe.R) uses seed 65, the checkout's vizier 0.6.2, uwot
0.2.4, igraph 2.3.1, edgebundle 1.0.1 and Plotly R 4.12.0. Each row measures the complete compact
`plotly_json(pretty = FALSE)` figure with the same 150 colored points and tooltips. Byte counts
exclude HTML, JavaScript libraries and compression. Coordinate slots include breaks.

| Iris view | Edge traces | Edge coordinate slots per axis | Figure JSON bytes |
| --- | ---: | ---: | ---: |
| Points only | 0 | 0 | 20,832 |
| Row 1's 22 incident edges, straight | 1 | 65 | 23,003 |
| All 1,347 edges, straight, one width | 1 | 4,040 | 139,324 |
| All straight, four membership-width bands | 4 | 4,037 | 139,909 |
| All straight, exact width by membership | 1,208 | 2,833 | 395,090 |
| All Hammer, one width | 1 | 68,696 | 2,646,853 |

Plotly scatter line width, color (including RGBA alpha) and trace opacity are scalar styles.
Each distinct combination needs a trace. Four width bands added 585 bytes here. Exact widths
required 1,208 traces even though shorter batches needed fewer path breaks. Bundling raised the
one-style figure size about 19-fold. None of these sizes measures browser latency. At 70,000
points, even one edge per point would produce 3.5 million Hammer vertices before breaks. Fourteen
outgoing choices per point would give 980,000 directed edges before merging or pruning. Those are
size estimates for that scenario.
[Plotly scatter attributes](https://plotly.com/r/reference/scatter/)

1. **Ship straight, static overlays in all three backends.** Support nearest, kth/furthest and all
   for ranked input, plus all edges for the sparse fitted graph or an ordinary table. Keep one
   uniform style across backends initially. Use Matrix optionally for sparse extraction. uwot,
   rnndescent and quadra supply example data. ggplot2 and Plotly stay optional.
2. **Make incident edges the first interactive feature.** Put the original row ID in point
   `customdata` before Plotly splits categories into traces. On click, look up both endpoints and
   replace one reusable straight-line trace with that neighborhood. For ranked data, retain which
   choices are outgoing and which are incoming. A standalone widget needs the coordinates and a
   compact adjacency index up front. A Shiny app can keep the graph server-side and send the
   selected edges. The row-1 measurement covers one displayed neighborhood only. A browser trial
   should measure initial load and click-to-draw on a larger graph before promising scale. Pan,
   zoom and point tooltips already work on a static widget and do not perform this lookup.
   [Plotly events](https://plotly.com/javascript/plotlyjs-events/)
3. **Offer Hammer later for small full-graph overviews.** Use optional igraph and edgebundle only
   on this path. The same paths can feed base, ggplot2 and Plotly, but the payload argues for
   starting with static base/ggplot2 output. Keep Plotly bundling and continuous per-edge styles
   off the initial feature list. Exact weights remain available in the edge table throughout.
