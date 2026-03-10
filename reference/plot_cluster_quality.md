# Plot silhouette scores across k values for a cluster_bouquet result

Draws a bar chart of the composite silhouette score (or plain mean
silhouette if `resolution = 0`) across all candidate k values evaluated
during automatic k selection. The selected k is highlighted. Use this to
inspect the elbow and decide whether a different k might be preferable.

## Usage

``` r
plot_cluster_quality(x, dark_mode = FALSE)
```

## Arguments

- x:

  A `cluster_bouquet` object returned by
  [`cluster_bouquet()`](https://mxnl.github.io/bouquets/reference/cluster_bouquet.md).

- dark_mode:

  Logical. Match the dark background of
  [`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md).
  Default `FALSE`.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object (invisibly), also printed to the device.

## Details

This function requires that
[`cluster_bouquet()`](https://mxnl.github.io/bouquets/reference/cluster_bouquet.md)
was called with `k = "auto"` – if a fixed k was supplied there are no
scores to plot.

## Examples

``` r
# \donttest{
set.seed(42)
n <- 52L
gw <- tibble::tibble(
  week    = rep(seq(as.Date("2023-01-01"), by = "week", length.out = n), 6L),
  station = rep(paste0("S", 1:6), each = n),
  level   = c(cumsum(rnorm(n)), cumsum(rnorm(n)), cumsum(rnorm(n)),
              cumsum(rnorm(n)), cumsum(rnorm(n)), cumsum(rnorm(n)))
)
result <- cluster_bouquet(gw, week, station, level)
plot_cluster_quality(result)

# }
```
