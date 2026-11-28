# Select Edges for an Embedding Plot

Prepare an ordinary edge table for the `overlays` argument of
[`embed_plot()`](https://jlmelville.github.io/vizier/reference/embed_plot.md),
[`embed_ggplot()`](https://jlmelville.github.io/vizier/reference/embed_ggplot.md)
or
[`embed_plotly()`](https://jlmelville.github.io/vizier/reference/embed_plotly.md).

## Usage

``` r
embed_edges(x, select = "all", k = NULL)
```

## Arguments

- x:

  A list with a ranked neighbor index matrix `idx` and optional matching
  distance matrix `dist`. Row and neighbor IDs refer to the same
  observations. Examples include a uwot metric block (`fit$nn[[1]]`) and
  a neighbor graph returned by rnndescent. Alternatively, supply a
  square, symmetric numeric sparse Matrix, such as a uwot `fit$fgraph`.

- select:

  For ranked neighbors, `"all"`, `"nearest"` (rank one), or `"furthest"`
  (the last retained input-space neighbor in each row). Sparse graphs
  support only `"all"` because directional ranks are lost.

- k:

  Optional positive integer. With `"all"`, keep up to the first `k`
  non-self neighbors per row. With `"furthest"`, take exactly rank `k`;
  every row must have that many neighbors.

## Value

A data frame with integer columns `from` and `to` indexing rows of the
original coordinates. Ranked input contributes directed edges with
`rank` and, if supplied, `distance`. Sparse input contributes a single
undirected edge per nonzero above the diagonal, with its value as
`weight`. The `n_vertices` attribute records the input observation
count.

## Details

Ranked indices are one-based and distances must be nondecreasing within
each row. Every occurrence of self is removed by ID. Other IDs retain
their first occurrence, including distinct neighbors tied at zero
distance. uwot normally counts self in its requested neighbor count.

## Examples

``` r
nn <- list(idx = rbind(c(1, 2, 3), c(2, 1, 3), c(3, 2, 1)))
edges <- embed_edges(nn, select = "nearest")
embed_plot(cbind(1:3, c(1, 3, 2)), colors = "navy", overlays = edges)
```
